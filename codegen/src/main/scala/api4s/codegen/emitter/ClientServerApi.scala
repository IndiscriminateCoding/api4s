package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._

import java.io.File

object ClientServerApi {
  object default extends ClientServerApi()

  def apply(pkg: String, file: File, api: Api): String = {
    import default._

    val eps = api.endpoints.values.flatMap(_.values)

    List(
      s"package $pkg",
      "",
      "import api4s.outputs._",
      "import api4s.RouteInfo",
      "import cats.~>",
      "import fs2.Pure",
      "import io.circe.Json",
      "import org.http4s.{ Media, Response }",
      "import org.http4s",
      "import shapeless.{ :+:, CNil }",
      "",
      s"import $pkg.Model._",
      "",
      s"trait Api[F[_]] {",
      eps.map(e => "  " + withDefaults(e)).mkString("\n"),
      "",
      s"  final def mapK[G[_]](f: RouteInfo => F ~> G): Api[G] = new Api.MapK(f, this)",
      "",
      s"  final def mapK[G[_]](f: F ~> G): Api[G] = mapK(_ => f)",
      "}",
      "",
      "object Api {",
      Routes(api, file).mkString("\n"),
      "",
      MapK(eps).mkString("\n"),
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

      s" = this.f(Api.$name)(this.api.$name$params)"
    }

    def apply(eps: Iterable[Endpoint]): List[String] = List(
      List(
        "private class MapK[F[_], G[_]](",
        "  f: RouteInfo => F ~> G,",
        "  api: Api[F]",
        ") extends Api[G] {"
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

class ClientServerApi(F: String = "F") {
  object utils extends Utils(F)

  import utils._

  private def convertParam(p: Parameter): String = p match {
    case Parameter(n, t, true) => s"$n: ${typeStr(t)}"
    case Parameter(n, t, false) => s"$n: Option[${typeStr(t)}]"
  }

  private def convertParamDefault(p: Parameter): String = p match {
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
