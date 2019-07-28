package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast.{ Response, Type }

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
    case class Specific(status: String, t: Option[Type]) extends ResponseType {
      def plain: String = typeStr(t)

      def lifted = s"F[$plain]"
    }
    case class Multi(rs: ListMap[String, Option[Type]]) extends ResponseType {
      def plain: String = s"${
        rs.map {
          case ("NoContent", _) => "NoContent"
          case (s, t) => s"$s[${typeStr(t)}]"
        }.mkString(" :+: ")
      } :+: CNil"

      def lifted = s"F[$plain]"
    }

    def apply(rs: SortedMap[Option[Int], Response]): ResponseType = {
      def statusName: PartialFunction[Int, String] = {
        case 200 => "Ok"
        case 201 => "Created"
        case 202 => "Accepted"
        case 204 => "NoContent"
      }

      val rs2xx = rs
        .filter(_._1.exists(statusName.isDefinedAt))
        .toList
        .map { case (c, r) => statusName(c.get) -> r.t }

      rs2xx match {
        case rs if rs.exists(_._2.contains(TFile())) => Untyped
        case Nil => Untyped
        case (s, t) :: Nil => Specific(s, t)
        case rs => Multi(ListMap(rs: _*))
      }
    }
  }

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
    case TFile() => "Stream[F, Byte]"
    case TObj(_) => "Map[String, Json]"
  }

  def typeStr(t: Option[Type]): String = t match {
    case None => "Unit"
    case Some(t) => typeStr(t)
  }
}
