package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._

object ClientServerApi {
  private def convertParam(p: Parameter): String = p match {
    case Parameter(n, t, true) => s"$n: ${typeStr(t)}"
    case Parameter(n, t, false) => s"$n: Option[${typeStr(t)}] = None"
  }

  private def convertParamDefault(p: Parameter): String = p match {
    case Parameter(n, t @ TMedia, true) => s"$n: ${typeStr(t)} = Media()"
    case Parameter(n, t @ TArr(_), true) => s"$n: ${typeStr(t)} = Nil"
    case Parameter(n, t, true) => s"$n: ${typeStr(t)}"
    case Parameter(n, t, false) => s"$n: Option[${typeStr(t)}] = None"
  }

  private def parameters(e: Endpoint): String =
    e.orderedParameters.map { case (_, p) => convertParam(p) } match {
      case Nil => ""
      case ps => s"(${ps.mkString(", ")})"
    }

  private def parametersDefault(e: Endpoint): String =
    e.orderedParameters.map { case (_, p) => convertParamDefault(p) } match {
      case Nil => ""
      case ps => s"(${ps.mkString(", ")})"
    }

  def withDefaults(e: Endpoint): String = {
    val endpointStr = s"def ${e.name.get}${parametersDefault(e)}"
    s"$endpointStr: ${producesLifted(e.produces)}"
  }

  def apply(e: Endpoint): String = {
    val endpointStr = s"def ${e.name.get}${parameters(e)}"
    s"$endpointStr: ${producesLifted(e.produces)}"
  }

  def apply(pkg: String, endpoints: Map[List[Segment], Map[Method, Endpoint]]): String = {
    val eps = endpoints.values.flatMap(_.values)
    List(
      s"package $pkg",
      "",
      "import api4s.runtime.Media",
      "import api4s.runtime.outputs._",
      "import cats.effect.Resource",
      "import fs2.Stream",
      "import io.circe.Json",
      "import org.http4s.Response",
      "import shapeless.{ :+:, CNil }",
      "",
      s"import $pkg.Model._",
      "",
      "trait Api[F[_]] {",
      eps.map(e => "  " + withDefaults(e)).mkString("\n"),
      "}"
    ).mkString("\n")
  }
}
