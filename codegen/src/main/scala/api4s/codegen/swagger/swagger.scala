package api4s.codegen.swagger

import api4s.codegen.Utils._
import api4s.codegen.ast.Parameter._
import api4s.codegen.ast.{ Parameter => Param, Response => Resp, _ }
import io.circe.Decoder.Result
import io.circe.Json
import org.http4s.MediaRange

import scala.collection.immutable.{ ListMap, SortedMap }

case class Root(
  info: Info,
  basePath: Option[String],
  consumes: Option[List[String]],
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
    .mapOnValues(_.resolve(
      responses.getOrElse(ListMap.empty),
      parameters.getOrElse(ListMap.empty),
      consumes,
      produces
    ))

  def endpoints: ListMap[List[Segment], ListMap[Method, Endpoint]] =
    resolvedPaths
      .map { case (path, item) =>
        val segments = PathParser(path)

        def partName(s: Segment.Simple): List[String] = s match {
          case Segment.Static(_) => Nil
          case Segment.Argument(n) => List(n)
        }

        val params = segments.flatMap {
          case s: Segment.Simple => partName(s)
          case Segment.Mixed(ps) => ps flatMap partName
        }.toSet

        val endpoints = item.endpoints
        endpoints.values foreach { e =>
          val pathParams = e.parameters.filter(_._1 == Path).map(_._2).toSet
          require(params == pathParams.map(_.name) && pathParams.forall(_.name.nonEmpty),
            s"Incompatible type parameters: ${e.name} $path / $pathParams")
        }
        segments -> endpoints
      }

  def types: ListMap[String, Type] = definitions.getOrElse(ListMap.empty).mapOnValues(_.getType)

  def api: Api = Api(info.version, types, endpoints)
}

object Root {
  def apply(s: String): Root = {
    import io.circe.generic.auto._
    import io.circe.yaml.parser.parse

    parse(s).fold(
      throw _,
      _.as[Root].fold(throw _, identity)
    )
  }
}

case class Info(version: String)

case class PathItem(
  ops: ListMap[Method, Operation],
  parameters: Option[List[Parameter]]
) {
  def resolve(
    rs: ListMap[String, Response],
    ps: ListMap[String, Parameter],
    c: Option[List[String]],
    p: Option[List[String]]
  ): PathItem = PathItem(
    ops = ops.mapOnValues(_
      .addParameters(parameters)
      .resolveResponses(rs)
      .resolveParameters(ps)
      .addMediaTypes(c, p)
    ),
    parameters = None
  )

  def endpoints: ListMap[Method, Endpoint] = ops.mapOnValues(_.endpoint)
}

object PathItem {
  import io.circe._

  private val methodOfString: PartialFunction[String, Method] = {
    case "get" => Method.Get
    case "post" => Method.Post
    case "put" => Method.Put
    case "delete" => Method.Delete
    case "options" => Method.Options
    case "head" => Method.Head
    case "patch" => Method.Patch
  }

  implicit val pathItemDecoder: Decoder[PathItem] = new Decoder[PathItem] {
    import cats.instances.all._
    import cats.syntax.traverse._
    import io.circe.generic.auto._

    def apply(c: HCursor): Result[PathItem] = for {
      flds <- Decoder[ListMap[String, Json]].apply(c)
      ps <- flds.get("parameters") match {
        case None => Right(None)
        case Some(ps) => ps.as[List[Parameter]].map(Some(_))
      }
      ops <- flds
        .filter { case (k, _) => methodOfString.isDefinedAt(k.toLowerCase) }
        .toList
        .traverse[Result, (Method, Operation)] { case (k, v) =>
          v.as[Operation].map(methodOfString(k) -> _)
        }
    } yield PathItem(
      ops = ListMap(ops: _*),
      parameters = ps
    )
  }
}

case class Operation(
  operationId: Option[String],
  consumes: Option[List[String]],
  produces: Option[List[String]],
  parameters: Option[List[Parameter]],
  responses: SortedMap[String, Response] // may contain default; at least one element
) {
  require(responses.nonEmpty, s"responses object is empty: $this ")

  def addMediaTypes(c: Option[List[String]], p: Option[List[String]]): Operation = this.copy(
    consumes = consumes.orElse(c),
    produces = produces.orElse(p)
  )

  def addParameters(ps: Option[List[Parameter]]): Operation = copy(
    parameters = Some(ps.getOrElse(Nil) ++ parameters.getOrElse(Nil))
  )

  def resolveResponses(rs: Map[String, Response]): Operation = copy(
    responses = responses.mapOnValues {
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
    val params = parameters.getOrElse(Nil).flatMap {
      case p if p.in.contains("header") => Some(Hdr(p.name.get) -> p.param)
      case p if p.in.contains("query") => Some(Query(p.name.get) -> p.param)
      case p if p.in.contains("path") => Some(Path -> p.param)
      case p => None
    }

    val requestBody = {
      val body = parameters.getOrElse(Nil).find(_.in.contains("body")).map(_.param)
      val form = parameters.getOrElse(Nil).filter(_.in.contains("formData")).map(_.param)
      val required = body.exists(_.required) || form.nonEmpty
      val consumeList = consumes.getOrElse(Nil) match {
        case Nil if body.nonEmpty => List("application/json")
        case Nil if form.nonEmpty => List("application/x-www-form-urlencoded")
        case cs => cs
      }

      val mediaRanges: ListMap[MediaRange, Type] = ListMap(consumeList
        .flatMap { c =>
          val mr = MediaRange.parse(c) match {
            case Left(e) => throw e
            case Right(r) => r
          }

          (body, form) match {
            case (None, Nil) => None
            case (Some(p), Nil) => Some(mr -> p.t)
            case (None, _ :: _) =>
              val flds = form.map { p =>
                p.name -> Type.Field(p.t, p.name, p.required)
              }
              Some(mr -> Type.TObj(ListMap(flds: _*)))
            case (Some(_), _ :: _) => throw new Exception("Both body and formData is defined!")
          }
        }: _*
      )

      RequestBody(name = body.map(_.name), ranges = mediaRanges, required = required)
    }

    val resps = {
      val mediaTypes = produces
        .getOrElse(List("application/json"))
        .map(mt => MediaRange.parse(mt) match {
          case Left(e) => throw e
          case Right(r) => r
        })

      def mediaTypeMap(r: Resp): ListMap[MediaRange, Resp] = ListMap(mediaTypes.map(_ -> r): _*)

      responses map {
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
    }

    Endpoint(
      name = operationId,
      requestBody = requestBody,
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
    case _ if `type`.contains("integer") && format.contains("int32") => Type.TInt
    case _ if `type`.contains("integer") => Type.TLong
    case _ if `type`.contains("number") && format.contains("float32") => Type.TFloat
    case _ if `type`.contains("number") => Type.TDouble
    case _ if `type`.contains("string") => Type.TString
    case _ if `type`.contains("boolean") => Type.TBool
    case _ if `type`.contains("array") && collectionFormat.contains("multi") && items.nonEmpty =>
      Type.TArr(items.get.getType)
    case _ if `type`.contains("array") && collectionFormat.contains("multi") =>
      Type.TArr(Type.TString)
    case _ if `type`.contains("array") && !in.contains("body") => Type.TString
    case _ if `type`.contains("array") && items.nonEmpty => Type.TArr(items.get.getType)
    case _ if `type`.contains("array") => Type.TArr(Type.TJson)
    case _ if `type`.contains("file") => Type.TMedia
    case _ if schema.nonEmpty => schema.get.getType
    case _ => throw new IllegalArgumentException(s"incorrect parameter: $this")
  }

  def param: Param = Param(name.get, getType, required.getOrElse(false))
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
    case _ if `type`.contains("integer") && format.contains("int32") => Type.TInt
    case _ if `type`.contains("integer") => Type.TLong
    case _ if `type`.contains("number") && format.contains("float32") => Type.TFloat
    case _ if `type`.contains("number") => Type.TDouble
    case _ if `type`.contains("string") => Type.TString
    case _ if `type`.contains("boolean") => Type.TBool
    case _ if `type`.contains("array") && items.nonEmpty => Type.TArr(items.get.getType)
    case _ if `type`.contains("array") => Type.TArr(Type.TJson)
    case _ if `type`.contains("file") => Type.TMedia
    case _ if `type`.contains("object") =>
      val _allOf = allOf.getOrElse(Nil)
      val _props = properties.getOrElse(ListMap.empty).toList
      (_allOf, discriminator, _props, additionalProperties) match {
        case (_, Some(_), _, _) => Type.TMap(Type.TJson)
        case (Nil, None, _, None) =>
          val fields = _props
            .map { case (k, v) =>
              k -> Type.Field(v.getType, k, required.exists(_.contains(k)))
            }
          Type.TObj(ListMap(fields: _*))
        case (Nil, None, Nil, Some(s)) => Type.TMap(s.getType)
        case (Nil, None, flds, Some(s)) =>
          val t = s.getType
          if (flds.forall(_._2.getType == t)) Type.TMap(t)
          else Type.TMap(Type.TJson)
        case (_ :: _, _, _, _) => Type.TMap(Type.TJson)
      }
    case _ if `type`.isEmpty => Type.TJson
    case _ => throw new IllegalArgumentException(s"incorrect schema: $this")
  }
}
