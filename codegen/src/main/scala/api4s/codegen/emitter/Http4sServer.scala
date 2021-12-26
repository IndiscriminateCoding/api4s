package api4s.codegen.emitter

import api4s.codegen.ast._
import org.http4s.util.StringWriter
import org.http4s.{ MediaRange, MediaType }

import scala.collection.immutable.ListMap

object Http4sServer {
  object utils extends Utils

  import utils._

  private def methodMatcher(m: Method, e: Endpoint): List[String] = {
    import Type._
    val name = e.name.get

    def primitiveStr(t: Type): String =
      if (primitive(t)) typeStr(t)
      else throw new Exception(s"Type ${typeStr(t)} isn't primitive (endpoint = $name)")

    val params = e.orderedParameters.map {
      case (Parameter.Path, Parameter(n, t, true)) =>
        s"""Decode[${primitiveStr(t)}].apply($n, "$n")"""
      case (Parameter.Hdr(rn), Parameter(_, t, req)) =>
        val ts = if (req) primitiveStr(t) else s"Option[${primitiveStr(t)}]"
        s"""Decode[$ts].apply(_request.headers, "$rn")"""
      case (Parameter.Query(rn), Parameter(_, TArr(t), _)) =>
        s"""Decode[List[${primitiveStr(t)}]].apply(_request.uri.query, "$rn")"""
      case (Parameter.Query(rn), Parameter(_, t, req)) =>
        val ts = if (req) primitiveStr(t) else s"Option[${primitiveStr(t)}]"
        s"""Decode[$ts].apply(_request.uri.query, "$rn")"""
      case (Parameter.Body(_), Parameter(n, TMedia, true)) =>
        s"$n.map($n => Media[Pure]($n, _request.headers))"
      case (Parameter.Body(_), Parameter(n, TMedia, false)) =>
        s"$n.map($n => $n.map($n => Media[Pure]($n, _request.headers)))"
      case (Parameter.Body(_), Parameter(n, _, _)) => n
      case (Parameter.InlinedBody(rn), Parameter(_, TArr(t), _)) =>
        s"""Decode[List[${primitiveStr(t)}]].apply(_formData, "$rn")"""
      case (Parameter.InlinedBody(rn), Parameter(_, t, req)) =>
        val ts = if (req) primitiveStr(t) else s"Option[${primitiveStr(t)}]"
        s"""Decode[$ts].apply(_formData, "$rn")"""
      case (pt, p) => throw new Exception(s"Unexpected parameter $p in $pt")
    }

    def emptyResponseMapper(c: String): String = s"Response[S](Status.$c)"
    def nonEmptyResponseMapper(c: String, mr: MediaRange, t: Type, on: String): String =
      (mr, t) match {
        case (mr, _) if MediaType.application.json.satisfiedBy(mr) =>
          s"Response[S](Status.$c).withEntity($on)(jsonEncoder)"
        case (mt: MediaType, TString) if MediaRange.`text/*`.satisfiedBy(mt) =>
          val sw = new StringWriter()
          MediaType.http4sHttpCodecForMediaType.render(sw, mt)
          s"""Response[S](Status.$c).withEntity($on)(Runtime.textEncoder("${sw.result}"))"""
        case _ => s"""Response[S](status = Status.$c, headers = $on.headers, body = $on.body)"""
      }

    val liftArgs = s"(_request.requestPrelude, Api.$name)"
    def apiMapper(on: String) = e.produces match {
      case Produces.Untyped => List(s"S.map(_L.lift$liftArgs($on))(_.covary[S])")
      case Produces.One(c, None) =>
        List(s"S.map(_L.lift$liftArgs($on))(_ => ${emptyResponseMapper(c)})")
      case Produces.One(c, Some((mr, t))) =>
        List(s"S.map(_L.lift$liftArgs($on))(x => ${nonEmptyResponseMapper(c, mr, t, "x")})")
      case Produces.Many(rs) =>
        val mapper = rs.toList.zipWithIndex.map {
          case ((c, None), i) => s"case ${shapelessPat(i, "_")} => ${emptyResponseMapper(c)}"
          case ((c, Some((mr, t))), i) =>
            s"case ${shapelessPat(i, s"$c(x)")} => ${nonEmptyResponseMapper(c, mr, t, "x")}"
        } :+ s"case ${shapelessCNil(rs.size)} => cnil.impossible"
        List(
          List(s"S.map(_L.lift$liftArgs($on)) {"),
          mapper.map("  " + _),
          List("}")
        ).flatten
    }

    val apiValidated =
      if (e.orderedParameters.isEmpty) apiMapper(s"api.$name")
      else
        List(
          List("_mapN("),
          params.map(p => s"  $p,"),
          List(
            s"  api.$name",
            ").fold[S[Response[S]]](",
            "  t => S.raiseError(Errors(t)),",
            "  x =>"
          ),
          apiMapper("x").map("    " + _),
          List(")"),
        ).flatten

    val apiCallWithBody = e.requestBody.consumes match {
      case Consumes.JsonBody(n, t) =>
        val opt = if (e.requestBody.required) "" else "Opt"
        List(
          List(s"Runtime.decodeValidated$opt[S, ${typeStr(t)}](_request)($n =>"),
          apiValidated.map("  " + _),
          List(s")(S, Runtime.jsonDecoder[S, ${typeStr(t)}])")
        ).flatten
      case Consumes.Entity(n, _) =>
        val opt = if (e.requestBody.required) "" else "Opt"
        List(
          List(s"Runtime.decodeValidated$opt[S, fs2.Stream[Pure, Byte]](_request)($n =>"),
          apiValidated.map("  " + _),
          List(s")(S, http4s.EntityDecoder.binary[S].map(fs2.Stream.chunk[Pure, Byte]))")
        ).flatten
      case Consumes.FormData(_) =>
        List(
          List(s"Runtime.decodeOrThrow[S, http4s.UrlForm](_request)(_formData =>"),
          apiValidated.map("  " + _),
          List(s")(S, http4s.UrlForm.entityDecoder[S])")
        ).flatten
      case Consumes.Empty => apiValidated
    }

    List(
      List(s"case Method.${m.toString.toUpperCase} =>"),
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

    s"case List($items) =>"
  }

  def apply(pkg: String, endpoints: ListMap[List[Segment], ListMap[Method, Endpoint]]): String = {
    val endpointList = endpoints.toList
      .sortBy {
        case (segments, _) => -segments.count(_.isInstanceOf[Segment.Static])
      }
      .flatMap { case (segment, methods) =>
        val allowed = methods
          .keys
          .map(m => s"Method.${m.toString.toUpperCase}")
          .mkString("Set(", ", ", ")")
        val methodList = methods
          .flatMap { case (m, e) => methodMatcher(m, e) }
          .toList :+ s"case _ => _router.methodNotAllowed($allowed)"

        List(
          List(segmentsMatcher(segment) ++ " _request.method match {"),
          methodList.map("  " + _),
          List("}")
        ).flatten
      }

    List(
      List(
        s"package $pkg",
        "",
        "import api4s._",
        "import api4s.Endpoint.Router",
        "import api4s.internal.Runtime",
        "import api4s.outputs._",
        "import api4s.utils.validated.{ MapN => _mapN }",
        "import cats.data.NonEmptyChain",
        "import cats.effect.Concurrent",
        "import fs2.Pure",
        "import io.circe.Json",
        "import org.http4s.{ Media, Method, Request, Response, Status }",
        "import org.http4s",
        "import shapeless.{ :+:, CNil, Coproduct, Inl, Inr }",
        "",
        s"import $pkg.Model._",
        "",
        "class Http4sServer[F[_], S[_]](",
        "  api: Api[F]",
        ")(implicit S: Concurrent[S], _L: LiftRoute[F, S]) extends Endpoint[S] {",
        "  def jsonEncoder[A : io.circe.Encoder]: http4s.EntityEncoder[S, A] =",
        "    http4s.circe.jsonEncoderWithPrinterOf[S, A](Runtime.printer)",
        "",
        "  def apply(",
        "    _request: Request[S],",
        "    _path: List[String],",
        "    _router: Router[S]",
        "  ): S[Response[S]] = _path match {"
      ),
      endpointList.map("    " + _),
      List(
        "    case _ => _router.notFound",
        "  }",
        "}"
      )
    ).flatten.mkString("\n")
  }
}
