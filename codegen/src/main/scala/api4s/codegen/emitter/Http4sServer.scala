package api4s.codegen.emitter

import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._
import org.http4s.util.StringWriter
import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.ListMap

object Http4sServer {
  private def methodMatcher(m: Method, e: Endpoint): List[String] = {
    import Type._

    def primitiveStr(t: Type): String =
      if (primitive(t)) typeStr(t)
      else throw new Exception(s"Type ${typeStr(t)} isn't primitive (endpoint = ${e.name.get})")

    val paramStr = {
      val ps = e.orderedParameters.map {
        case (Parameter.Path, Parameter(n, TString, true)) => n
        case (Parameter.Path, Parameter(n, t, true)) =>
          s"Helpers.parser[${primitiveStr(t)}].required($n)"
        case (Parameter.Hdr(rn), Parameter(_, t, req)) =>
          s"""request.header${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")"""
        case (Parameter.Query(rn), Parameter(_, TArr(t), _)) =>
          s"""request.queries[${primitiveStr(t)}]("$rn")"""
        case (Parameter.Query(rn), Parameter(_, t, req)) =>
          s"""request.query${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")"""
        case (Parameter.Body(_), Parameter(_, TMedia, _)) => "Media(request)"
        case (Parameter.Body(_), Parameter(n, _, _)) => n
        case (Parameter.InlinedBody(rn), Parameter(_, TArr(t), _)) =>
          s"""_formData.values.params[${primitiveStr(t)}]("$rn")"""
        case (Parameter.InlinedBody(rn), Parameter(_, t, req)) =>
          s"""_formData.values.param${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")"""
        case (pt, p) => throw new Exception(s"Unexpected parameter $p in $pt")
      }

      ps.mkString(", ")
    }

    val apiCall = s"api.${e.name.get}${if (paramStr.isEmpty) "" else s"($paramStr)"}"

    def responseMapperStr(c: String, t: Option[(MediaType, Type)]): String = t match {
      case None => s"Helpers.emptyResponse[F](Status.$c)"
      case Some((mt, t)) if MediaType.application.json.satisfiedBy(mt) =>
        s"Helpers.jsonResponse[F, ${typeStr(t)}](Status.$c)"
      case Some((mt, _)) if MediaRange.`text/*`.satisfiedBy(mt) =>
        val sw = new StringWriter()
        MediaType.http4sHttpCodecForMediaType.render(sw, mt)
        s"""Helpers.textResponse[F](Status.$c, "${sw.result}")"""
      case Some(_) =>
        s"Helpers.mediaResponse[F](Status.$c)"
    }

    val apiWithExtractor = e.produces match {
      case Produces.Untyped =>
        List(
          s"F.map($apiCall.allocated) {",
          "  case (x, r) => x.withBodyStream(x.body.onFinalize(r))",
          "}"
        )
      case Produces.One(c, t @ None) =>
        List(s"F.map($apiCall)(_ => ${responseMapperStr(c, t)})")
      case Produces.One(c, t @ Some(_)) =>
        List(s"F.map($apiCall)(${responseMapperStr(c, t)})")
      case Produces.Many(rs) =>
        val mapper = rs.toList.zipWithIndex.map {
          case ((c, t @ None), i) => s"case ${shapelessPat(i, "r")} => ${responseMapperStr(c, t)}"
          case ((c, t @ Some(_)), i) =>
            s"case ${shapelessPat(i, "r")} => ${responseMapperStr(c, t)}(r.content)"
        } :+ "case Inr(Inr(cnil)) => cnil.impossible"
        List(
          List(s"F.map($apiCall) {"),
          mapper.map("  " + _),
          List("}")
        ).flatten
    }
    val apiCallWithBody = e.requestBody.consumes match {
      case Consumes.JsonBody(n, t) if e.requestBody.required =>
        val decoder = s"Helpers.circeEntityDecoder[F, ${typeStr(t)}]"
        List(
          List(s"request.decodeWith($decoder, true)($n =>"),
          apiWithExtractor.map("  " + _),
          List(")")
        ).flatten
      case Consumes.JsonBody(n, t) =>
        List(
          List(s"request.decodeJsonOpt[${typeStr(t)}]($n => "),
          apiWithExtractor.map("  " + _),
          List(")")
        ).flatten
      case Consumes.FormData(_) =>
        val decoder = s"http4s.UrlForm.entityDecoder[F]"
        List(
          List(s"request.decodeWith($decoder, true)(_formData => "),
          apiWithExtractor.map("  " + _),
          List(")")
        ).flatten
      case Consumes.Entity(_, _) => apiWithExtractor
      case Consumes.Empty => apiWithExtractor
    }

    s"case Method.${m.toString.toUpperCase} =>" :: apiCallWithBody.map("  " + _)
  }

  private def segmentsMatcher(s: List[Segment]): String = {
    val items = s.map {
      case Segment.Static(p) => s""""$p""""
      case Segment.Argument(p) => p
    }.mkString(", ")

    s"case List($items) =>"
  }

  def apply(pkg: String, endpoints: ListMap[List[Segment], ListMap[Method, Endpoint]]): String = {
    val endpointList = endpoints.flatMap { case (segment, methods) =>
      val allowed = methods.keys.map(m => s"Method.${m.toString.toUpperCase}").mkString(", ")
      val methodList = methods
        .flatMap { case (m, e) => methodMatcher(m, e) }
        .toList :+ s"case _ => RoutingErrorAlgebra.methodNotAllowed($allowed)"

      List(
        List(segmentsMatcher(segment)),
        List(s"  def _apply = request.method match {"),
        methodList.map("    " + _),
        List("  }"),
        List("  _apply")
      ).flatten
    }.toList

    List(
      List(
        s"package $pkg",
        "",
        "import api4s.{ Endpoint, Media }",
        "import api4s.Endpoint.RoutingErrorAlgebra",
        "import api4s.internal.Helpers",
        "import api4s.internal.Helpers.{ RichRequest, RichUrlForm }",
        "import api4s.outputs._",
        "import cats.effect.{ Resource, Sync }",
        "import org.http4s.{ Method, Request, Response, Status }",
        "import org.http4s",
        "import shapeless.{ Inl, Inr }",
        "",
        s"import $pkg.Model._",
        "",
        "class Http4sServer[F[_]](api: Api[F])(implicit F: Sync[F]) extends Endpoint[F] {",
        "  protected def apply(req: Request[F])(R: RoutingErrorAlgebra[F]): F[Response[F]] =",
        "    try { _apply(req)(R) }",
        "    catch { case Helpers.RequestValidationError =>",
        "      F.pure(Response(status = Status.BadRequest))",
        "    }",
        "",
        "  private[this] def _apply(request: Request[F])(",
        "    RoutingErrorAlgebra: RoutingErrorAlgebra[F]",
        "  ): F[Response[F]] = request.pathSegments match {"
      ),
      endpointList.map("    " + _),
      List(
        "    case _ => RoutingErrorAlgebra.notFound",
        "  }",
        "}"
      )
    ).flatten.mkString("\n")
  }
}
