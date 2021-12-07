package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._

object ClientServerApi {
  object default extends ClientServerApi()

  def apply(pkg: String, endpoints: Map[List[Segment], Map[Method, Endpoint]]): String = {
    import default._

    val eps = endpoints.values.flatMap(_.values)

    List(
      s"package $pkg",
      "",
      "import api4s.outputs._",
      "import cats.~>",
      "import cats.effect.{ MonadCancel, Resource }",
      "import io.circe.Json",
      "import org.http4s.{ Media, Response }",
      "import org.http4s",
      "import shapeless.{ :+:, CNil }",
      "",
      s"import $pkg.Model._",
      "",
      "trait Api[F[_], S[_]] {",
      eps.map(e => "  " + withDefaults(e)).mkString("\n"),
      "",
      "  final def mapK[G[_]](f: F ~> G)(implicit F: MonadCancel[F, _], G: MonadCancel[G, _])" +
        ": Api[G, S] = new Api.MapK(f, this)",
      "}",
      "",
      MapK(eps).mkString("\n")
    ).mkString("\n")
  }

  private object MapK {
    object utils extends ClientServerApi(F = "G")

    def map(e: Endpoint): String = {
      val params = e.orderedParameters.map(_._2.name) match {
        case Nil => ""
        case ps => s"(${ps.mkString(", ")})"
      }
      " = " + (e.produces match {
        case Produces.Untyped => s"this.api.${e.name.get}$params.mapK(this.f)"
        case _ => s"this.f(this.api.${e.name.get}$params)"
      })
    }

    def apply(eps: Iterable[Endpoint]): List[String] = List(
      "object Api {",
      "  private class MapK[F[_], G[_], S[_]](",
      "    f: F ~> G,",
      "    api: Api[F, S]",
      "  )(implicit F: MonadCancel[F, _], G: MonadCancel[G, _]) extends Api[G, S] {",
      eps.map(e => "    " + utils(e) + map(e)).mkString("\n"),
      "  }",
      "}"
    )
  }
}

class ClientServerApi(F: String = "F", S: String = "S") {
  object utils extends Utils(F, S)
  import utils._

  private def convertParam(p: Parameter): String = p match {
    case Parameter(n, t, true) => s"$n: ${typeStr(t)}"
    case Parameter(n, t, false) => s"$n: Option[${typeStr(t)}]"
  }

  private def convertParamDefault(p: Parameter): String = p match {
    case Parameter(n, t @ TMedia, true) =>
      s"$n: ${typeStr(t)} = Media(http4s.EmptyBody, http4s.Headers.empty)"
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
}
