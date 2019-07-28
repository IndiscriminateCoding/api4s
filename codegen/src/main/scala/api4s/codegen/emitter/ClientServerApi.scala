package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._

object ClientServerApi {
  private def parameter(p: (ParameterType, Parameter)): String = p match {
    case (_, Parameter(name, _, t @ TArr(_), true)) => s"$name: ${typeStr(t)} = Nil"
    case (ParameterType.Query, Parameter(name, _, t @ TArr(_), false)) =>
      s"$name: ${typeStr(t)} = Nil"
    case (_, Parameter(name, _, t, true)) => s"$name: ${typeStr(t)}"
    case (_, Parameter(name, _, t, false)) => s"$name: Option[${typeStr(t)}] = None"
  }

  private def parameters(ps: List[(ParameterType, Parameter)]): String = ps match {
    case Nil => ""
    case _ => s"(${ps.map(parameter).mkString(", ")})"
  }

  def apply(e: Endpoint): String = {
    val endpointStr = s"def ${e.name.get}${parameters(e.parameters)}"
    s"$endpointStr: ${ResponseType(e.responses).lifted}"
  }

  def apply(pkg: String, endpoints: Map[List[Segment], Map[Method, Endpoint]]): String = {
    val eps = endpoints.values.flatMap(_.values)
    List(
      s"package $pkg",
      "",
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
      eps.map(e => "  " + apply(e)).mkString("\n"),
      "}"
    ).mkString("\n")
  }
}
