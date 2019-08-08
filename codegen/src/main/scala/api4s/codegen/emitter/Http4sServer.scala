package api4s.codegen.emitter

import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._
import org.http4s.MediaType
import org.http4s.util.StringWriter

import scala.collection.immutable.ListMap

object Http4sServer {
  private def methodMatcher(m: Method, e: Endpoint): List[String] = {
    import ParameterType._
    import Type._

    val primitives = Set[Type](TString(), TInt(), TLong(), TBool())

    def primitiveStr(t: Type): String =
      if (primitives(t)) typeStr(t)
      else throw new Exception(s"Type ${typeStr(t)} isn't primitive (endpoint = ${e.name.get})")

    val params = e.parameters
      .map {
        case (Path, Parameter(n, _, TString(), true)) => n -> true
        case (Path, Parameter(n, _, t, true)) =>
          s"Helpers.parser[${primitiveStr(t)}].required($n)" -> true
        case (Hdr, Parameter(_, rn, t, req)) =>
          s"""request.header${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")""" -> req
        case (Query, Parameter(_, rn, TArr(t), req)) =>
          s"""request.queries[${primitiveStr(t)}]("$rn")""" -> true
        case (Query, Parameter(_, rn, t, req)) =>
          s"""request.query${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")""" -> req
        case (pt, p) => throw new Exception(s"Unexpected parameter $p in $pt")
      }

    val rbt = RequestBodyType(e.requestBody)

    val requestBodyParams = rbt match {
      case RequestBodyType.JsonBody(n, _) => List(n -> e.requestBody.required)
      case RequestBodyType.FormData(flds) => flds.map {
        case (_, Field(TArr(t), rn, req)) =>
          s"""_formData.values.params[${primitiveStr(t)}]("$rn")""" -> req
        case (_, Field(t, rn, req)) =>
          s"""_formData.values.param${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")""" -> req
      }
      case RequestBodyType.Raw(_) => List("request.body" -> e.requestBody.required)
      case RequestBodyType.Empty => Nil
    }
    val paramStr = {
      val (req, opt) = (params ++ requestBodyParams).partition(_._2)
      (req ++ opt).map(_._1).mkString(", ")
    }

    val apiCall = s"api.${e.name.get}${if (paramStr.isEmpty) "" else s"($paramStr)"}"

    def responseMapperStr(c: String, t: Option[(MediaType, Type)]): String = t match {
      case None => s"Helpers.emptyResponse[F](Status.$c)"
      case Some((mt, t)) if isJson(mt) =>
        s"Helpers.jsonResponse[F, ${typeStr(t)}](Status.$c)"
      case Some((mt, _)) if isText(mt) =>
        val sw = new StringWriter()
        MediaType.http4sHttpCodecForMediaType.render(sw, mt)
        s"""Helpers.textResponse[F](Status.$c, "${sw.result}")"""
      case Some((mt, _)) =>
        val sw = new StringWriter()
        MediaType.http4sHttpCodecForMediaType.render(sw, mt)
        s"""Helpers.byteResponse[F](Status.$c, "${sw.result}")"""
    }

    val apiWithExtractor = ResponseType(e.responses) match {
      case ResponseType.Untyped => List(apiCall)
      case ResponseType.Specific(c, t @ None) =>
        List(s"F.map($apiCall)(_ => ${responseMapperStr(c, t)})")
      case ResponseType.Specific(c, t @ Some(_)) =>
        List(s"F.map($apiCall)(${responseMapperStr(c, t)})")
      case ResponseType.Multi(rs) =>
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
    val apiCallWithBody = rbt match {
      case RequestBodyType.JsonBody(n, t) if e.requestBody.required =>
        val decoder = s"Helpers.circeEntityDecoder[F, ${typeStr(t)}]"
        List(
          List(s"request.decodeWith($decoder, true)($n =>"),
          apiWithExtractor.map("  " + _),
          List(")")
        ).flatten
      case RequestBodyType.JsonBody(n, t) =>
        List(
          List(s"request.decodeJsonOpt[${typeStr(t)}]($n => "),
          apiWithExtractor.map("  " + _),
          List(")")
        ).flatten
      case RequestBodyType.FormData(_) =>
        val decoder = s"http4s.UrlForm.entityDecoder[F]"
        List(
          List(s"request.decodeWith($decoder, true)(_formData => "),
          apiWithExtractor.map("  " + _),
          List(")")
        ).flatten
      case _ => apiWithExtractor
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
      val methodList = methods
        .flatMap { case (m, e) => methodMatcher(m, e) }
        .toList :+ "case _ => RoutingErrorAlgebra.methodNotAllowed"

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
        "import api4s.runtime.Endpoint",
        "import api4s.runtime.Endpoint.RoutingErrorAlgebra",
        "import api4s.runtime.internal.Helpers",
        "import api4s.runtime.internal.Helpers.{ RichRequest, RichUrlForm }",
        "import api4s.runtime.outputs._",
        "import cats.effect.Sync",
        "import org.http4s.{ Method, Request, Response, Status }",
        "import org.http4s",
        "import shapeless.{ Inl, Inr }",
        "",
        s"import $pkg.Model._",
        "",
        "class Http4sServer[F[_]](api: Api[F])(implicit F: Sync[F]) extends Endpoint[F] {",
        "  protected def apply(req: Request[F])(R: RoutingErrorAlgebra[F]): F[Response[F]] =",
        "    try { _apply(req)(R) }",
        "    catch { case Helpers.RequestValidationError => R.badRequest }",
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
