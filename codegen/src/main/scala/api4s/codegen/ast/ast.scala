package api4s.codegen.ast

import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.{ ListMap, SortedMap }

case class Api(
  version: String,
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
  case object TFloat extends Type
  case object TDouble extends Type
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
  sealed trait Simple extends Segment
  case class Static(value: String) extends Simple
  case class Argument(name: String) extends Simple

  case class Mixed(parts: List[Simple]) extends Segment
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
  tags: List[String],
  requestBody: RequestBody,
  parameters: List[(Parameter.Kind, Parameter)],
  responses: SortedMap[Option[Int], ListMap[MediaRange, Response]]
) {

  import api4s.codegen.utils.Registry.registry

  responses foreach {
    case (Some(c), rs) =>
      def canHaveEntity = registry.get(c).fold(true) { case (_, allowed) => allowed }
      def entityExists = rs.values.exists(_.t.nonEmpty)

      if (!canHaveEntity && entityExists)
        throw new IllegalArgumentException(s"Non-empty $c response for endpoint $name")
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
      case (k@Parameter.Query(_), Parameter(n, t@Type.TArr(_), false)) =>
        k -> Parameter(n, t, true) // array parameters in a query shouldn't be wrapped in Option
      case (k@Parameter.Body(_), Parameter(n, t@Type.TMedia, false)) =>
        k -> Parameter(n, t, true) // no media is just empty Media
      case p => p
    }

    def haveDefault(p: Parameter): Boolean = p match {
      case Parameter(_, _, false) => true
      case Parameter(_, Type.TArr(_), _) => true
      case _ => false
    }

    val (a, b) = setRequired.partition { case (_, p) => haveDefault(p) }

    b ++ a
  }

  lazy val produces: Produces = {
    import Produces._

    val typed = responses.forall { case (_, rs) =>
      rs.forall { case (mr, r) => mr.isInstanceOf[MediaType] && !r.haveHeaders } &&
        rs.size <= 1
    } && responses.exists(_._1.exists(registry.contains))

    def getMediaType(rs: ListMap[MediaRange, Response]): Option[(MediaType, Type)] = for {
      (mr, r) <- rs.headOption
      mt = mr.asInstanceOf[MediaType]
      rt <- r.t
    } yield mt -> Type(mt, rt)

    () match {
      case _ if responses.contains(None) => Untyped
      case _ if !typed =>
        responses.toList match {
          case (Some(s), _) :: Nil =>
            registry.get(s).map {
              case (s, _) => One(s, Some(MediaRange.`*/*` -> Type.TMedia))
            }.getOrElse(Untyped)
          case _ => Untyped
        }
      case _ =>
        val filtered = responses.filter(_._1.exists(registry.contains))
        if (filtered.size > 1)
          Many(ListMap(filtered.toList.map {
            case (st, rs) => registry(st.get)._1 -> getMediaType(rs)
          }: _*))
        else {
          val (st, rs) = filtered.head
          One(registry(st.get)._1, getMediaType(rs))
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
    case JsonBody(name, _) => Some(name)
    case _ => None
  }

  lazy val consumes: Consumes = ranges.toList match {
    case Nil => Empty
    case (mr, t) :: Nil if mr == MediaType.application.json => JsonBody(name.get, t)
    case (mr, TObj(flds)) :: Nil if mr == MediaType.application.`x-www-form-urlencoded` =>
      FormData(flds.toList)
    case (mt: MediaType, _) :: Nil => Entity(name.getOrElse("media"), Some(mt))
    case _ => Entity(name.getOrElse("media"), None)
  }
}

case class Response(t: Option[Type], haveHeaders: Boolean)
