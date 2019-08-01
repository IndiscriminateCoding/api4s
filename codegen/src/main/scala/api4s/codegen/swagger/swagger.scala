package api4s.codegen.swagger

import api4s.codegen.Utils._
import api4s.codegen.ast.{ Parameter => Param, Response => Resp, _ }
import io.circe.Json
import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.{ ListMap, SortedMap }

case class Root(
  basePath: Option[String],
  produces: Option[List[String]],
  paths: Option[ListMap[String, PathItem]],
  definitions: Option[ListMap[String, Schema]],
  parameters: Option[ListMap[String, Parameter]],
  responses: Option[ListMap[String, Response]]
) {
  responses foreach { rs =>
    rs foreach { case (_, r) =>
      require(r.$ref.isEmpty, s"response definition references another response: $r")
    }
  }

  private[this] def insertBasePath: Root = copy(
    basePath = None,
    paths = basePath match {
      case Some(p) => paths.map(_.map { case (k, v) => s"$p$k" -> v })
      case None => paths
    }
  )

  private[this] def resolvedPaths: ListMap[String, PathItem] = insertBasePath
    .paths
    .getOrElse(ListMap.empty)
    .mapValueList(_.resolve(
      responses.getOrElse(ListMap.empty),
      parameters.getOrElse(ListMap.empty),
      produces
    ))

  def endpoints: ListMap[List[Segment], ListMap[Method, Endpoint]] =
    resolvedPaths
      .map { case (path, item) =>
        var params: Set[String] = Set()
        val segments = path.split("/").toList.filter(_.nonEmpty).map {
          case s if s.startsWith("{") =>
            val name = s.substring(1, s.length - 1)
            params = params + name
            Segment.Argument(name)
          case s => Segment.Static(s)
        }
        val endpoints = item.endpoints
        endpoints.values foreach { e =>
          val pathParams = e.parameters.filter(_._1 == ParameterType.Path).map(_._2).toSet
          require(params == pathParams.map(_.name),
            s"Incompatible type parameters: ${e.name} $path / $pathParams")
        }
        segments -> endpoints
      }

  def types: ListMap[String, Type] = definitions.getOrElse(ListMap.empty).mapValueList(_.getType)

  def api: Api = Api(types, endpoints)
}

object Root {
  def apply(s: String): Root = {
    import io.circe.generic.auto._
    import io.circe.yaml.parser.parse

    parse(s)
      .right.get
      .as[Root]
      .right.get
  }
}

case class PathItem(
  get: Option[Operation],
  post: Option[Operation],
  put: Option[Operation],
  delete: Option[Operation],
  options: Option[Operation],
  head: Option[Operation],
  patch: Option[Operation],
  parameters: Option[List[Parameter]]
) {
  private[this] lazy val insertParameters: PathItem = PathItem(
    get = get.map(_.addParameters(parameters)),
    post = post.map(_.addParameters(parameters)),
    put = put.map(_.addParameters(parameters)),
    delete = delete.map(_.addParameters(parameters)),
    options = options.map(_.addParameters(parameters)),
    head = head.map(_.addParameters(parameters)),
    patch = patch.map(_.addParameters(parameters)),
    parameters = None
  )

  def resolve(
    rs: ListMap[String, Response],
    ps: ListMap[String, Parameter],
    p: Option[List[String]]
  ): PathItem =
    insertParameters.copy(
      get = insertParameters.get
        .map(_.resolveParameters(ps).resolveResponses(rs).addProduces(p)),
      post = insertParameters.post
        .map(_.resolveParameters(ps).resolveResponses(rs).addProduces(p)),
      put = insertParameters.put
        .map(_.resolveParameters(ps).resolveResponses(rs).addProduces(p)),
      delete = insertParameters.delete
        .map(_.resolveParameters(ps).resolveResponses(rs).addProduces(p)),
      options = insertParameters.options
        .map(_.resolveParameters(ps).resolveResponses(rs).addProduces(p)),
      head = insertParameters.head
        .map(_.resolveParameters(ps).resolveResponses(rs).addProduces(p)),
      patch = insertParameters.patch
        .map(_.resolveParameters(ps).resolveResponses(rs).addProduces(p)),
    )

  def endpoints: ListMap[Method, Endpoint] = ListMap(List(
    get.map(Method.Get -> _.endpoint),
    post.map(Method.Post -> _.endpoint),
    put.map(Method.Put -> _.endpoint),
    delete.map(Method.Delete -> _.endpoint),
    options.map(Method.Options -> _.endpoint),
    head.map(Method.Head -> _.endpoint),
    patch.map(Method.Patch -> _.endpoint)
  ).filter(_.nonEmpty).map(_.get): _*)
}

case class Operation(
  operationId: Option[String],
  produces: Option[List[String]],
  parameters: Option[List[Parameter]],
  responses: SortedMap[String, Response] // may contain default; at least one element
) {
  require(responses.nonEmpty, s"responses object is empty: $this")

  def addProduces(p: Option[List[String]]): Operation = this.copy(
    produces = produces.orElse(p)
  )

  def addParameters(ps: Option[List[Parameter]]): Operation = copy(
    parameters = Some(ps.getOrElse(Nil) ++ parameters.getOrElse(Nil))
  )

  def resolveResponses(rs: Map[String, Response]): Operation = copy(
    responses = responses.mapValues {
      case r if r.$ref.nonEmpty => rs(r.$ref.get.stripPrefix("#/responses/"))
      case r => r
    }
  )

  def resolveParameters(ps: ListMap[String, Parameter]): Operation = copy(
    parameters = parameters.map(_.map {
      case p if p.$ref.nonEmpty => ps(p.$ref.get.stripPrefix("#/parameters/"))
      case p => p
    })
  )

  def pathParams: ListMap[String, Param] = ListMap(
    parameters.getOrElse(Nil)
      .filter(_.in.contains("path"))
      .map(p => p.name.get -> p.param): _*
  )

  def endpoint: Endpoint = {
    val params = parameters.getOrElse(Nil).map {
      case p if p.in.contains("header") => ParameterType.Hdr -> p.param
      case p if p.in.contains("body") => ParameterType.Body -> p.param
      case p if p.in.contains("formData") => ParameterType.FormData -> p.param
      case p if p.in.contains("query") => ParameterType.Query -> p.param
      case p if p.in.contains("path") => ParameterType.Path -> p.param
      case p => throw new IllegalArgumentException(s"Unexpected parameter $p")
    }

    val mediaTypes = produces
      .getOrElse(List("application/json"))
      .map(mt => MediaRange.parse(mt) match {
        case Left(e) => throw e
        case Right(r) => r
      })

    def mediaTypeMap(r: Resp): ListMap[MediaRange, Resp] = ListMap(mediaTypes.map(_ -> r): _*)

    val resps = responses map {
      case ("default", r) =>
        None -> mediaTypeMap(Resp(r.schema.map(_.getType), r.headers.exists(_.nonEmpty)))
      case (c, r) =>
        try {
          Some(c.toInt) ->
            mediaTypeMap(Resp(r.schema.map(_.getType), r.headers.exists(_.nonEmpty)))
        } catch {
          case _: NumberFormatException =>
            throw new IllegalArgumentException(s"incorrect response '$r'")
        }
    }
    Endpoint(
      name = operationId,
      parameters = params,
      responses = resps
    )
  }
}

case class Parameter(
  name: Option[String],
  in: Option[String],
  $ref: Option[String],
  schema: Option[Schema],
  `type`: Option[String],
  format: Option[String],
  allowEmptyValue: Option[Boolean],
  items: Option[Schema],
  collectionFormat: Option[String],
  default: Option[Json],
  required: Option[Boolean]
) {
  // TODO: items: Option[Items]
  def getType: Type = () match {
    case _ if `type`.contains("integer") && format.contains("int32") => Type.TInt()
    case _ if `type`.contains("integer") => Type.TLong()
    case _ if `type`.contains("number") => Type.TNum()
    case _ if `type`.contains("string") => Type.TString()
    case _ if `type`.contains("boolean") => Type.TBool()
    case _ if `type`.contains("array") && collectionFormat.contains("multi") && items.nonEmpty =>
      Type.TArr(items.get.getType)
    case _ if `type`.contains("array") && collectionFormat.contains("multi") =>
      Type.TArr(Type.TString())
    case _ if `type`.contains("array") && !in.contains("body") => Type.TString()
    case _ if `type`.contains("array") && items.nonEmpty => Type.TArr(items.get.getType)
    case _ if `type`.contains("array") => Type.TArr(Type.TJson())
    case _ if `type`.contains("file") => Type.TBinary()
    case _ if schema.nonEmpty => schema.get.getType
    case _ => throw new IllegalArgumentException(s"incorrect parameter: $this")
  }

  def param: Param = Param(name.get, name.get, getType, required.getOrElse(false))
}

case class Response(
  $ref: Option[String],
  schema: Option[Schema],
  headers: Option[Map[String, Json]]
)

case class Schema(
  $ref: Option[String],
  format: Option[String],
  required: Option[List[String]],
  `type`: Option[String],
  allOf: Option[List[Schema]],
  discriminator: Option[String],
  items: Option[Schema],
  readOnly: Option[Boolean],
  properties: Option[ListMap[String, Schema]],
  additionalProperties: Option[Schema]
) {
  def getType: Type = () match {
    case _ if $ref.nonEmpty => Type.TRef($ref.get.stripPrefix("#/definitions/"))
    case _ if `type`.contains("integer") && format.contains("int32") => Type.TInt()
    case _ if `type`.contains("integer") => Type.TLong()
    case _ if `type`.contains("number") => Type.TNum()
    case _ if `type`.contains("string") => Type.TString()
    case _ if `type`.contains("boolean") => Type.TBool()
    case _ if `type`.contains("array") && items.nonEmpty => Type.TArr(items.get.getType)
    case _ if `type`.contains("array") => Type.TArr(Type.TJson())
    case _ if `type`.contains("file") => Type.TBinary()
    case _ if `type`.contains("object")
      && additionalProperties.isEmpty
      && allOf.isEmpty
      && properties.nonEmpty =>
      val props = properties.get
        .map { case (k, v) =>
          k -> Type.Field(v.getType, required.exists(_.contains(k)))
        }
      Type.TObj(props)
    case _ if `type`.contains("object")
      && additionalProperties.nonEmpty
      && !properties.exists(_.nonEmpty) => Type.TMap(additionalProperties.get.getType)
    case _ if `type`.contains("object") && allOf.nonEmpty && discriminator.isEmpty =>
      val types = allOf.get.map(_.getType)
      val fields = types.foldLeft[ListMap[String, Type.Field]](ListMap.empty) {
        case (a, Type.TObj(flds)) =>
          val res = a ++ flds
          require(a.size == res.size + flds.size, s"Overlapping fields in allOf: $types")
          res
        case _ => throw new IllegalArgumentException(s"using allOf on non-object types: $this")
      }
      Type.TObj(fields)
    case _ if `type`.contains("object") && properties.isEmpty && additionalProperties.isEmpty =>
      Type.TMap(Type.TJson())
    case _ if `type`.isEmpty => Type.TJson()
    case _ => throw new IllegalArgumentException(s"incorrect schema: $this")
  }
}
