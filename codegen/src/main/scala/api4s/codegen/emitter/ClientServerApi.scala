package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._

object ClientServerApi {
  private def params(ps: List[(ParameterType, Parameter)]): List[(Boolean, String, Type)] =
    ps map {
      case (ParameterType.Query(_), Parameter(n, t @ TArr(_), false)) => (true, n, t)
      case (_, p) => (p.required, p.name, p.t)
    }

  private def requestBodyParams(r: RequestBody): List[(Boolean, String, Type)] =
    RequestBodyType(r) match {
      case RequestBodyType.Empty => Nil
      case RequestBodyType.Raw(n) => List((r.required, n, TBinary))
      case RequestBodyType.JsonBody(n, t) => List((r.required, n, t))
      case RequestBodyType.FormData(flds) => flds map {
        case (n, Field(t, _, req)) => (req, n, t)
      }
    }

  private def convertParam(p: (Boolean, String, Type)): String = p match {
    case (true, n, t @ TArr(_)) => s"$n: ${typeStr(t)} = Nil"
    case (true, n, t) => s"$n: ${typeStr(t)}"
    case (false, n, t) => s"$n: Option[${typeStr(t)}] = None"
  }

  private def reorderParams(p: List[(Boolean, String, Type)]): List[(Boolean, String, Type)] = {
    val (req, opt) = p.partition(_._1)
    req ++ opt
  }

  private def parameters(e: Endpoint): String = {
    val ps = reorderParams(params(e.parameters) ++ requestBodyParams(e.requestBody))
      .map(convertParam)
    ps match {
      case Nil => ""
      case _ => s"(${ps.mkString(", ")})"
    }
  }

  def apply(e: Endpoint): String = {
    val endpointStr = s"def ${e.name.get}${parameters(e)}"
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
