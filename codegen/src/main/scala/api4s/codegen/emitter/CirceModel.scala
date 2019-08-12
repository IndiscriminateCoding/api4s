package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._

object CirceModel {
  def apply(pkg: String, ts: Map[String, Type]): String = {
    val defs = ts
      .map { case (n, t) => typeDef(n, t).map("  " + _).mkString("\n") }
      .mkString("\n")
    val codecs = ts.map {
      case (n, TObj(_)) => List(
        s"  implicit val encoderOf$n: Encoder[$n] = deriveEncoder",
        s"  implicit val decoderOf$n: Decoder[$n] = deriveDecoder"
      )
      case _ => Nil
    }.filter(_.nonEmpty).map(_.mkString("\n")).mkString("\n\n")
    List(
      s"package $pkg",
      "",
      "import io.circe.{ Decoder, Encoder, Json }",
      "import io.circe.generic.semiauto._",
      "",
      "object Model {",
      defs,
      "",
      codecs,
      "}"
    ).mkString("\n")
  }

  private def typeDef(n: String, t: Type): List[String] = t match {
    case TObj(fields) =>
      val fieldStrs = fields.map {
        case (n, Field(t @ TArr(_), _, true)) => s"  $n: ${typeStr(t)} = Nil"
        case (n, Field(t, _, true)) => s"  $n: ${typeStr(t)}"
        case (n, Field(t, _, false)) => s"  $n: Option[${typeStr(t)}] = None"
      }.toList match {
        case Nil => Nil
        case List(e) => List(e)
        case l2 =>
          val rev = l2.reverse
          (rev.head :: rev.tail.map(_ + ",")).reverse
      }
      s"case class $n(" :: fieldStrs ++ List(")")
    case _ => List(s"type $n = ${typeStr(t)}")
  }
}
