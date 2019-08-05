package api4s.codegen.emitter

import api4s.codegen.Utils._
import api4s.codegen.ast.Type._
import api4s.codegen.ast.{ RequestBody, Response, Type }
import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.{ ListMap, SortedMap }

object Utils {
  sealed trait ResponseType {
    def plain: String
    def lifted: String = s"F[$plain]"
  }

  object ResponseType {
    case object Untyped extends ResponseType {
      def plain = "Response[F]"
    }
    case class Specific(status: String, t: Option[(MediaType, Type)]) extends ResponseType {
      def plain: String = typeStr(t)
    }
    case class Multi(rs: ListMap[String, Option[(MediaType, Type)]]) extends ResponseType {
      def plain: String = s"${
        rs.map {
          case ("NoContent", _) => "NoContent"
          case (s, t) => s"$s[${typeStr(t)}]"
        }.mkString(" :+: ")
      } :+: CNil"
    }

    def apply(rs: SortedMap[Option[Int], ListMap[MediaRange, Response]]): ResponseType = {
      def statusName: PartialFunction[Int, String] = {
        case 200 => "Ok"
        case 201 => "Created"
        case 202 => "Accepted"
        case 204 => "NoContent"
      }

      def toMt(r: List[(MediaRange, Response)]): Option[(MediaType, Type)] = for {
        (mr, r) <- r.headOption
        mt = mr.asInstanceOf[MediaType]
        rt <- r.t
      } yield mt -> rt

      val rs2xx = rs
        .filter(_._1.exists(statusName.isDefinedAt))
        .toList
        .map { case (c, r) => statusName(c.get) -> r.toList }

      rs2xx match {
        case Nil => Untyped
        case _ if rs.values.exists(_.values.exists(_.haveHeaders)) => Untyped
        case rs if rs.exists(_._2.size > 1) => Untyped
        case rs if rs.exists(_._2.exists(!_._1.isInstanceOf[MediaType])) => Untyped
        case (s, t) :: Nil => Specific(s, toMt(t))
        case rs => Multi(ListMap(rs: _*).mapValueList(toMt))
      }
    }
  }

  sealed trait RequestBodyType
  object RequestBodyType {
    case object Empty extends RequestBodyType
    case class Raw(name: String) extends RequestBodyType
    case class FormData(flds: List[(String, Field)]) extends RequestBodyType
    case class JsonBody(name: String, t: Type) extends RequestBodyType

    def apply(r: RequestBody): RequestBodyType =
      r.ranges.toList match {
        case Nil => Empty
        case (mr, t) :: Nil if isJson(mr) => JsonBody(r.name.get, t)
        case (_, TObj(flds)) :: _
          if r.ranges.keys.forall(isFormData) && r.ranges.values.toSet.size == 1 =>
          FormData(flds.toList)
        case _ => Raw(r.name.getOrElse("_body"))
      }
  }

  def isJson(mr: MediaRange): Boolean = mr match {
    case mt: MediaType => mt.mainType == "application" && mt.subType == "json"
    case _ => false
  }

  def isText(mr: MediaRange): Boolean = mr.mainType == "text"

  def isBinary(mr: MediaRange): Boolean = !isJson(mr) && !isText(mr)

  def isFormData(mr: MediaRange): Boolean = mr match {
    case mt: MediaType => mt == MediaType.application.`x-www-form-urlencoded`
    //case _ => mr.mainType == "multipart" // TODO
    case _ => false
  }

  def shapelessPat(i: Int, v: String): String =
    if (i == 0) s"Inl($v)"
    else s"Inr(${shapelessPat(i - 1, v)})"

  def typeStr(t: Type): String = t match {
    case TRef(name) => name
    case TMap(et) => s"Map[String, ${typeStr(et)}]"
    case TArr(it) => s"List[${typeStr(it)}]"
    case TJson() => "Json"
    case TInt() => "Int"
    case TLong() => "Long"
    case TNum() => "Double"
    case TString() => "String"
    case TBool() => "Boolean"
    case TBinary() => "Stream[F, Byte]"
    case TObj(_) => "Map[String, Json]"
  }

  def typeStr(t: (MediaRange, Type)): String = t match {
    case (mt: MediaType, t) if isJson(mt) => typeStr(t)
    case (mt: MediaType, _) if isText(mt) => typeStr(TString())
    case _ => typeStr(TBinary())
  }

  def typeStr(t: Option[(MediaRange, Type)]): String = t.fold("Unit")(typeStr)
}
