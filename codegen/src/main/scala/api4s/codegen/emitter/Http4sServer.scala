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

    val paramStr = e.parameters
      .map {
        case (Body, Parameter(name, _, _, _)) => name
        case (FormData, Parameter("formData", _, TBinary(), _)) => "request.body"
        case (Path, Parameter(n, _, TString(), true)) => n
        case (Path, Parameter(n, _, t, true)) =>
          s"Helpers.parser[${primitiveStr(t)}].required($n)"
        case (Hdr, Parameter(_, rn, t, req)) =>
          s"""request.header${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")"""
        case (Query, Parameter(_, rn, TArr(t), req)) =>
          s"""request.queries[${primitiveStr(t)}]("$rn")"""
        case (Query, Parameter(_, rn, t, req)) =>
          s"""request.query${if (req) "" else "Opt"}[${primitiveStr(t)}]("$rn")"""
        case (pt, p) => throw new Exception(s"Unexpected parameter $p in $pt")
      }.mkString(", ")

    val apiCall = s"api.${e.name.get}${if (paramStr.isEmpty) "" else s"($paramStr)"}"

    def responseMapperStr(c: String, t: Option[(MediaType, Type)]): String = t match {
      case None => s"Helpers.emptyResponse[F](Status.$c)"
      case Some((mt, t)) if isJson(mt) =>
        s"Helpers.jsonResponse[F, ${typeStr(t)}](Status.$c)"
      case Some((mt, TString())) if isText(mt) =>
        val sw = new StringWriter()
        MediaType.http4sHttpCodecForMediaType.render(sw, mt)
        s"""Helpers.textResponse[F](Status.$c, "${sw.result}")"""
      case Some((mt, TBinary())) =>
        val sw = new StringWriter()
        MediaType.http4sHttpCodecForMediaType.render(sw, mt)
        s"""Helpers.byteResponse[F](Status.$c, "${sw.result}")"""
      case Some((mt, t)) => throw new Exception(s"[S] Unexpected media-type $mt for type $t")
    }

    val apiWithExtractor = ResponseType(e.responses) match {
      case ResponseType.Untyped => List(
        s"F.map($apiCall.allocated){ case (x, r) => x.withBodyStream(x.body.onFinalize(r)) }"
      )
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
    val apiCallWithBody = e.parameters.find(_._1 == Body) match {
      case None => apiWithExtractor
      case Some((_, Parameter(n, rn, t, true))) =>
        val decoder = s"Helpers.circeEntityDecoder[F, ${typeStr(t)}]"
        List(
          List(s"request.decodeWith($decoder, true)($n => "),
          apiWithExtractor.map("  " + _),
          List(")")
        ).flatten
      case Some((_, Parameter(_, _, _, false))) =>
        throw new Exception("optional body isn't expected!")
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
        "import api4s.runtime.internal.Helpers.RichRequest",
        "import api4s.runtime.outputs._",
        "import cats.effect.Sync",
        "import org.http4s.{ Method, Request, Response, Status }",
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
