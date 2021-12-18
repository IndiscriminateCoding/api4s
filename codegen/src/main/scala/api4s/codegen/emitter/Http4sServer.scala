package api4s.codegen.emitter

import api4s.codegen.ast._
import org.http4s.util.StringWriter
import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.ListMap

object Http4sServer {
  object utils extends Utils(S = "F")

  import utils._

  private def methodMatcher(m: Method, e: Endpoint): List[String] = {
    import Type._
    val name = e.name.get

    def primitiveStr(t: Type): String =
      if (primitive(t)) typeStr(t)
      else throw new Exception(s"Type ${typeStr(t)} isn't primitive (endpoint = $name)")

    val params = e.orderedParameters.map {
      case (Parameter.Path, Parameter(n, t, true)) =>
        s"""Decode[${primitiveStr(t)}]($n, "$n")"""
      case (Parameter.Hdr(rn), Parameter(_, t, req)) =>
        val ts = if (req) primitiveStr(t) else s"Option[${primitiveStr(t)}]"
        s"""Decode[$ts](request.headers, "$rn")"""
      case (Parameter.Query(rn), Parameter(_, TArr(t), _)) =>
        s"""Decode[List[${primitiveStr(t)}]](request.uri.query, "$rn")"""
      case (Parameter.Query(rn), Parameter(_, t, req)) =>
        val ts = if (req) primitiveStr(t) else s"Option[${primitiveStr(t)}]"
        s"""Decode[$ts](request.uri.query, "$rn")"""
      case (Parameter.Body(_), Parameter(_, TMedia, _)) =>
        "cats.data.Validated.Valid[Media[F]](request)"
      case (Parameter.Body(_), Parameter(n, _, _)) => n
      case (Parameter.InlinedBody(rn), Parameter(_, TArr(t), _)) =>
        s"""Decode[List[${primitiveStr(t)}]](_fromData, "$rn")"""
      case (Parameter.InlinedBody(rn), Parameter(_, t, req)) =>
        val ts = if (req) primitiveStr(t) else s"Option[${primitiveStr(t)}]"
        s"""Decode[$ts](_formData, "$rn")"""
      case (pt, p) => throw new Exception(s"Unexpected parameter $p in $pt")
    }

    def responseMapperStr(c: String, t: Option[(MediaType, Type)]): String = t match {
      case None => s"Helpers.emptyResponse[F](Status.$c)"
      case Some((mt, t)) if MediaType.application.json.satisfiedBy(mt) =>
        s"Helpers.jsonResponse[F, ${typeStr(t)}](Status.$c)"
      case Some((mt, TString)) if MediaRange.`text/*`.satisfiedBy(mt) =>
        val sw = new StringWriter()
        MediaType.http4sHttpCodecForMediaType.render(sw, mt)
        s"""Helpers.textResponse[F](Status.$c, "${sw.result}")"""
      case Some(_) => s"Helpers.mediaResponse[F](Status.$c)"
    }

    def apiMapper(on: String) = e.produces match {
      case Produces.Untyped =>
        List(
          s"F.map($on.allocated) {",
          "  case (x, r) => x.withBodyStream(x.body.onFinalize(r))",
          "}"
        )
      case Produces.One(c, t@None) =>
        List(s"F.map($on)(_ => ${responseMapperStr(c, t)})")
      case Produces.One(c, t@Some(_)) =>
        List(s"F.map($on)(${responseMapperStr(c, t)})")
      case Produces.Many(rs) =>
        val mapper = rs.toList.zipWithIndex.map {
          case ((c, t@None), i) => s"case ${shapelessPat(i, "r")} => ${responseMapperStr(c, t)}"
          case ((c, t@Some(_)), i) =>
            s"case ${shapelessPat(i, "r")} => ${responseMapperStr(c, t)}(r.content)"
        } :+ s"case ${shapelessCNil(rs.size)} => cnil.impossible"
        List(
          List(s"F.map($on) {"),
          mapper.map("  " + _),
          List("}")
        ).flatten
    }

    val callRouter =
      if (e.orderedParameters.isEmpty)
        List(
          List(
            "Router.response(",
            s"  Api.$name,",
          ),
          apiMapper(s"api.$name").map("  " + _),
          List(")")
        ).flatten
      else List(
        List(
          s"val _badRequest = (t: NonEmptyChain[Throwable]) =>",
          s"  Router.badRequest(Api.$name, Errors(t))",
          s"val _route = (x: ${producesLifted(e.produces)}) =>",
          "  Router.response(",
          s"    Api.$name,"
        ),
        apiMapper("x").map("    " + _),
        List("  )"),
      ).flatten

    val apiValidated =
      if (e.orderedParameters.isEmpty) Nil
      else
        List(
          List("_validatedMapN("),
          params.map(p => s"  $p,"),
          List(
            s"  api.$name",
            ").fold(_badRequest, _route)"
          )
        ).flatten

    val apiCallWithBody = e.requestBody.consumes match {
      case Consumes.JsonBody(n, t) =>
        val opt = if (e.requestBody.required) "" else "Opt"
        List(
          List(s"request.decodeValidated$opt[${typeStr(t)}]($n =>"),
          apiValidated.map("  " + _),
          List(s")(F, Helpers.circeEntityDecoder[F, ${typeStr(t)}])")
        ).flatten
      case Consumes.FormData(_) =>
        List(
          List(s"request.decode[http4s.UrlForm](_formData =>"),
          apiValidated.map("  " + _),
          List(s")(F, http4s.UrlForm.entityDecoder[F])")
        ).flatten
      case Consumes.Entity(_, _) => apiValidated
      case Consumes.Empty => apiValidated
    }

    List(
      List(s"case Method.${m.toString.toUpperCase} =>"),
      callRouter.map("  " + _),
      apiCallWithBody.map("  " + _)
    ).flatten
  }

  private def segmentsMatcher(s: List[Segment]): String = {
    val items = s.map {
      case Segment.Static(p) => s""""$p""""
      case Segment.Argument(p) => p
      case Segment.Mixed(_) =>
        throw new Exception("Mixed segments are not supported in http4s-server")
    }.mkString(", ")

    s"case Vector($items) =>"
  }

  def apply(pkg: String, endpoints: ListMap[List[Segment], ListMap[Method, Endpoint]]): String = {
    val endpointList = endpoints.toList
      .sortBy {
        case (segments, _) => -segments.count(_.isInstanceOf[Segment.Static])
      }
      .flatMap { case (segment, methods) =>
        val allowed = methods.keys.map(m => s"Method.${m.toString.toUpperCase}").mkString(", ")
        val methodList = methods
          .flatMap { case (m, e) => methodMatcher(m, e) }
          .toList :+ s"case _ => Router.methodNotAllowed($allowed)"

        List(
          List(segmentsMatcher(segment)),
          List(s"  def _apply = request.method match {"),
          methodList.map("    " + _),
          List("  }"),
          List("  _apply")
        ).flatten
      }
    val streaming = endpoints.values.exists(_.values.exists(needStreaming))
    val api = if (streaming) "Api[F, F]" else "Api[F]"

    List(
      List(
        s"package $pkg",
        "",
        "import api4s._",
        "import api4s.Endpoint.Router",
        "import api4s.internal.Helpers",
        "import api4s.internal.Helpers.RichRequest",
        "import api4s.outputs._",
        "import api4s.utils.validated.{ MapN => _validatedMapN }",
        "import cats.data.NonEmptyChain",
        "import cats.effect.{ Async, Resource }",
        "import io.circe.Json",
        "import org.http4s.{ Media, Method, Request, Response, Status }",
        "import org.http4s",
        "import shapeless.{ :+:, CNil, Coproduct, Inl, Inr }",
        "",
        s"import $pkg.Model._",
        "",
        s"class Http4sServer[F[_]](api: $api)(implicit F: Async[F]) extends Endpoint[F] {",
        "  def apply(request: Request[F])(",
        "    Router: Router[F]",
        "  ): F[Response[F]] = request.pathSegments match {"
      ),
      endpointList.map("    " + _),
      List(
        "    case _ => Router.notFound",
        "  }",
        "}"
      )
    ).flatten.mkString("\n")
  }
}
