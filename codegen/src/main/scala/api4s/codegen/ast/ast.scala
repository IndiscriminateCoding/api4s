package api4s.codegen.ast

import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.{ ListMap, SortedMap }

case class Api(
  types: ListMap[String, Type],
  endpoints: ListMap[List[Segment], ListMap[Method, Endpoint]]
)

sealed trait Type
object Type {
  case class Field(t: Type, rn: String, required: Boolean)

  case class TMap(t: Type) extends Type
  case class TRef(name: String) extends Type
  case class TObj(fields: ListMap[String, Field]) extends Type
  case class TArr(items: Type) extends Type
  case object TJson extends Type
  case object TInt extends Type
  case object TLong extends Type
  case object TNum extends Type
  case object TString extends Type
  case object TBool extends Type
  case object TMedia extends Type

  def apply(mr: MediaRange, t: Type): Type = (mr, t) match {
    case (_, TMedia) => t
    case (mt: MediaType, t) if MediaType.application.json.satisfiedBy(mt) => t
    case (mr, _) if MediaRange.`text/*`.satisfiedBy(mr) => TString
    case _ => TMedia
  }
}

sealed trait Method
object Method {
  case object Get extends Method
  case object Post extends Method
  case object Put extends Method
  case object Delete extends Method
  case object Options extends Method
  case object Head extends Method
  case object Patch extends Method
}

sealed trait Segment
object Segment {
  case class Static(value: String) extends Segment
  case class Argument(name: String) extends Segment
}

case class Parameter(name: String, t: Type, required: Boolean)
object Parameter {
  sealed trait Kind

  case class Body(name: String) extends Kind
  case class InlinedBody(name: String) extends Kind
  case class Hdr(name: String) extends Kind
  case class Query(name: String) extends Kind
  case object Path extends Kind
}

case class Endpoint(
  name: Option[String],
  requestBody: RequestBody,
  parameters: List[(Parameter.Kind, Parameter)],
  responses: SortedMap[Option[Int], ListMap[MediaRange, Response]]
) {
  responses.getOrElse(Some(204), ListMap.empty) foreach {
    case (_, Response(Some(_), _)) =>
      throw new IllegalArgumentException(s"Non-empty 204 response for endpoint $name")
    case _ =>
  }

  lazy val orderedParameters: List[(Parameter.Kind, Parameter)] = {
    val all = parameters ++ (requestBody.consumes match {
      case Consumes.Empty => Nil
      case Consumes.JsonBody(name, t) =>
        List(Parameter.Body(name) -> Parameter(name, t, requestBody.required))
      case Consumes.Entity(name, _) =>
        List(Parameter.Body(name) -> Parameter(name, Type.TMedia, requestBody.required))
      case Consumes.FormData(flds) => flds.map { case (name, Type.Field(t, rn, req)) =>
        Parameter.InlinedBody(rn) -> Parameter(name, t, req)
      }
    })
    val setRequired = all map {
      case (k @ Parameter.Query(_), Parameter(n, t @ Type.TArr(_), false)) =>
        k -> Parameter(n, t, true)
      case (k @ Parameter.Body(_), Parameter(n, t @ Type.TMedia, false)) =>
        k -> Parameter(n, t, true)
      case p => p
    }

    def haveDefault(p: Parameter): Boolean = p match {
      case Parameter(_, _, false) => true
      case Parameter(_, Type.TMedia, _) => true
      case Parameter(_, Type.TArr(_), _) => true
      case _ => false
    }

    val (a, b) = setRequired.partition { case (_, p) => haveDefault(p) }

    b ++ a
  }

  lazy val produces: Produces = {
    import Produces._

    val known = Map(
      200 -> "Ok",
      201 -> "Created",
      202 -> "Accepted",
      204 -> "NoContent"
    )

    val typed = responses.forall { case (_, rs) =>
      rs.forall { case (mr, r) => mr.isInstanceOf[MediaType] && !r.haveHeaders } &&
        rs.size <= 1
    } && responses.exists(_._1.exists(known.contains))

    def getMediaType(rs: ListMap[MediaRange, Response]): Option[(MediaType, Type)] = for {
      (mr, r) <- rs.headOption
      mt = mr.asInstanceOf[MediaType]
      rt <- r.t
    } yield mt -> Type(mt, rt)

    () match {
      case _ if responses.contains(None) => Untyped
      case _ if !typed => Untyped
      case _ =>
        val filtered = responses.filter(_._1.exists(known.contains))
        if (filtered.size > 1)
          Many(ListMap(filtered.toList.map {
            case (st, rs) => known(st.get) -> getMediaType(rs)
          }: _*))
        else {
          val (st, rs) = filtered.head
          One(known(st.get), getMediaType(rs))
        }
    }
  }
}

case class RequestBody(
  name: Option[String],
  ranges: ListMap[MediaRange, Type],
  required: Boolean
) {
  import Consumes._
  import api4s.codegen.ast.Type.TObj

  lazy val proposedName: Option[String] = consumes match {
    case Entity(name, _) => Some(name)
    case _ => None
  }

  lazy val consumes: Consumes = ranges.toList match {
    case Nil => Empty
    case (mr, t) :: Nil if mr == MediaType.application.json => JsonBody(name.get, t)
    case (mr, TObj(flds)) :: Nil if mr == MediaType.application.`x-www-form-urlencoded` =>
      FormData(flds.toList)
    case (mt: MediaType, _) :: Nil =>
      Entity(name.getOrElse(s"${mt.mainType}/${mt.subType}"), Some(mt))
    case _ => Entity(name.getOrElse("entity"), None)
  }
}

case class Response(t: Option[Type], haveHeaders: Boolean)
