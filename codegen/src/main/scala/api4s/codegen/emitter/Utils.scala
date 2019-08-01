package api4s.codegen.emitter

import api4s.codegen.Utils._
import api4s.codegen.ast.Type._
import api4s.codegen.ast.{ Response, Type }
import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.{ ListMap, SortedMap }

object Utils {
  sealed trait ResponseType {
    def plain: String
    def lifted: String
  }

  object ResponseType {
    case object Untyped extends ResponseType {
      def plain = "Resource[F, Response[F]]"

      def lifted: String = plain
    }
    case class Specific(status: String, t: Option[(MediaType, Type)]) extends ResponseType {
      def plain: String = typeStr(t)

      def lifted = s"F[$plain]"
    }
    case class Multi(rs: ListMap[String, Option[(MediaType, Type)]]) extends ResponseType {
      def plain: String = s"${
        rs.map {
          case ("NoContent", _) => "NoContent"
          case (s, t) => s"$s[${typeStr(t)}]"
        }.mkString(" :+: ")
      } :+: CNil"

      def lifted = s"F[$plain]"
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
        rt <- () match {
          case _ if isGeneric(mt) => r.t.map(_ => TBinary())
          case _ if isText(mt) => r.t.map(_ => TString())
          case _ => r.t
        }
      } yield mt -> rt

      val rs2xx = rs
        .filter(_._1.exists(statusName.isDefinedAt))
        .toList
        .map { case (c, r) => statusName(c.get) -> r.toList }

      rs2xx match {
        case Nil => Untyped
        case rs if rs.exists(_._2.size > 1) => Untyped
        case rs if rs.exists(_._2.exists(!_._1.isInstanceOf[MediaType])) => Untyped
        case (s, t) :: Nil => Specific(s, toMt(t))
        case rs => Multi(ListMap(rs: _*).mapValueList(toMt))
      }
    }
  }

  def isJson(mt: MediaType): Boolean = mt.mainType == "application" && mt.subType == "json"

  def isText(mt: MediaType): Boolean = mt.mainType == "text"

  def isGeneric(mt: MediaType): Boolean = !isJson(mt) && !isText(mt)

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

  def typeStr(t: Option[(MediaRange, Type)]): String = t match {
    case None => "Unit"
    case Some((mt: MediaType, t)) if mt.mainType == "application" && mt.subType == "json" =>
      typeStr(t)
    case Some((mt: MediaType, TString())) if mt.mainType == "text" => typeStr(TString())
    case Some(_) => typeStr(TBinary())
  }
}
