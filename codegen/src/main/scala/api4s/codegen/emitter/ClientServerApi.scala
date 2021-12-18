package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._

import java.io.File

object ClientServerApi {
  object default extends ClientServerApi()

  def apply(pkg: String, file: File, api: Api): String = {
    import default._

    val eps = api.endpoints.values.flatMap(_.values)
    val streaming = eps.exists(utils.needStreaming)
    def declApi(f: String, s: String): String = if (streaming) s"Api[$f, $s]" else s"Api[$f]"

    List(
      s"package $pkg",
      "",
      "import api4s.outputs._",
      "import api4s.RouteInfo",
      "import cats.~>",
      "import cats.effect.{ MonadCancel, Resource }",
      "import io.circe.Json",
      "import org.http4s.{ Media, Response }",
      "import org.http4s",
      "import shapeless.{ :+:, CNil }",
      "",
      s"import $pkg.Model._",
      "",
      s"trait ${declApi("F[_]", "S[_]")} {",
      eps.map(e => "  " + withDefaults(e)).mkString("\n"),
      "",
      s"  final def mapK[G[_]](f: RouteInfo => F ~> G)(implicit F: MonadCancel[F, _]," +
        s" G: MonadCancel[G, _]): ${declApi("G", "S")} = new Api.MapK(f, this)",
      "",
      "  final def mapK[G[_]](f: F ~> G)(implicit F: MonadCancel[F, _], G: MonadCancel[G, _])" +
        s": ${declApi("G", "S")} = mapK(_ => f)",
      "}",
      "",
      "object Api {",
      Routes(api, file).mkString("\n"),
      "",
      MapK(eps, streaming).mkString("\n"),
      "}"
    ).mkString("\n")
  }

  private object MapK {
    object utils extends ClientServerApi(F = "G")

    def map(e: Endpoint): String = {
      val name = e.name.get
      val params = e.orderedParameters.map(_._2.name) match {
        case Nil => ""
        case ps => s"(${ps.mkString(", ")})"
      }

      " = " + (e.produces match {
        case Produces.Untyped => s"this.api.$name$params.mapK(this.f(Api.$name))"
        case _ => s"this.f(Api.$name)(this.api.$name$params)"
      })
    }

    def apply(eps: Iterable[Endpoint], streaming: Boolean): List[String] = List(
      List(
        s"private class MapK[F[_], G[_]${if (streaming) ", S[_]" else ""}](",
        "  f: RouteInfo => F ~> G,",
        s"  api: Api[F${if (streaming) ", S" else ""}]",
        ")(implicit F: MonadCancel[F, _], G: MonadCancel[G, _]) extends " +
          s"Api[G${if (streaming) ", S" else ""}] {"
      ),
      eps.map(e => "  " + utils(e) + map(e)),
      List("}")
    ).flatten.map("  " ++ _)
  }

  private object Routes {
    def apply(api: Api, file: File): List[String] = {
      val eps = api.endpoints.values.flatMap(_.values)
      val cachedTags = scala.collection.mutable.Map[Set[String], String]()
      val endpoints = eps.map { ep =>
        val name = ep.name.get
        val tags = ep.tags.map(t => s""""$t"""").toSet
        val tagStr = cachedTags.get(tags) match {
          case Some(tag) => s"$tag.tags"
          case None =>
            cachedTags.put(tags, name)
            tags.mkString("Set(", ", ", ")")
        }
        s"""val $name = RouteInfo("${file.getName}", "$name", $tagStr)"""
      }
      val all = s"val _all: Set[RouteInfo] = Set(${eps.map(_.name.get).mkString(", ")})"

      endpoints.toList :+ all
    }.map("  " + _)
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
    case Parameter(n, t@TMedia, true) =>
      s"$n: ${typeStr(t)} = Media(http4s.EmptyBody, http4s.Headers.empty)"
    case Parameter(n, t@TArr(_), true) => s"$n: ${typeStr(t)} = Nil"
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
