package api4s.codegen.emitter

import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._

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
        case (FormData, Parameter("formData", _, TFile(), _)) => "request.body"
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
    val apiWithExtractor = ResponseType(e.responses) match {
      case ResponseType.Untyped =>
        s"F.map($apiCall.allocated){ case (x, r) => x.withBodyStream(x.body.onFinalize(r)) }"
      case ResponseType.Specific(c, None) =>
        s"F.map($apiCall)(_ => Helpers.emptyResponse[F](Status.$c))"
      case ResponseType.Specific(c, Some(t)) =>
        s"F.map($apiCall)(Helpers.jsonResponse[F, ${typeStr(t)}](Status.$c))"
      case ResponseType.Multi(_) => s"F.map($apiCall)(_.fold(ToResponse))"
    }
    val apiCallWithBody = e.parameters.find(_._1 == Body) match {
      case None => apiWithExtractor
      case Some((_, Parameter(n, rn, t, true))) =>
        val decoder = s"Helpers.decoder[F, ${typeStr(t)}]"
        s"request.decodeWith($decoder, true)($n => $apiWithExtractor)"
      case Some((_, Parameter(_, _, _, false))) =>
        throw new Exception("optional body isn't expected!")
    }

    List(s"case Method.${m.toString.toUpperCase} => $apiCallWithBody")
  }

  private def segmentsMatcher(s: List[Segment]): String = {
    val items = s.map {
      case Segment.Static(p) => s""""$p""""
      case Segment.Parameter(p) => p
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
        "",
        s"import $pkg.Model._",
        "",
        "class Http4sServer[F[_]](api: Api[F])(implicit F: Sync[F]) extends Endpoint[F] {",
        "  private[this] object ToResponse extends ToResponse[F]",
        "",
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
