package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import org.http4s.{ MediaRange, MediaType }

object Http4sClient {
  object clientServerApi extends ClientServerApi(S = "F")
  import clientServerApi.utils._

  private def endpoint(segments: List[Segment], method: Method, e: Endpoint): List[String] = {
    def addToString(t: Type): String = t match {
      case TString => ""
      case t if primitive(t) => ".toString"
      case _ => throw new Exception(s"can't convert type $t to String")
    }

    val queryRequired = e.parameters.exists {
      case (Parameter.Query(_), _) => true
      case _ => false
    }

    val params = e.parameters flatMap {
      case (Parameter.Query(rn), Parameter(n, TArr(t), _)) =>
        List(s"""$n foreach (x => _query = _query :+ ("$rn" -> Some(x${addToString(t)})))""")
      case (Parameter.Query(rn), Parameter(n, t, false)) =>
        List(s"""$n foreach (x => _query = _query :+ ("$rn" -> Some(x${addToString(t)})))""")
      case (Parameter.Hdr(rn), Parameter(n, t, false)) =>
        List(s"""$n foreach (x => _hdrs = ("$rn", x${addToString(t)}) :: _hdrs)""")
      case _ => Nil
    }

    val requiredQueryParams = e.parameters.filter {
      case (Parameter.Query(_), Parameter(_, TArr(_), _)) => false
      case (Parameter.Query(_), p) => p.required
      case _ => false
    }.map {
      case (Parameter.Query(rn), Parameter(n, t, _)) => s""""$rn" -> Some($n${addToString(t)})"""
      case _ => throw new Exception("never happens")
    }.mkString(", ")

    val requiredHdrParams = {
      val res = e.parameters.filter {
        case (Parameter.Hdr(_), p) => p.required
        case _ => false
      }.map {
        case (Parameter.Hdr(rn), Parameter(n, t, _)) =>
          s"""("$rn", $n${addToString(t)})"""
        case _ => throw new Exception("never happens")
      }

      val needsZeroContentLength: Set[Method] = Set(Method.Patch, Method.Post, Method.Put)
      if (e.requestBody.consumes == Consumes.Empty && needsZeroContentLength(method))
        """("Content-Length", "0")""" :: res
      else res
      }.mkString(", ")

    def segmentPath(parts: List[Segment.Simple]): String = parts.map {
      case Segment.Static(s) => s
      case Segment.Argument(p) =>
        val needEncode = e.parameters.exists {
          case (Parameter.Path, Parameter(n, TString, _)) if n == p => true
          case _ => false
        }
        if (needEncode) s"$${Uri.pathEncode($p)}"
        else s"$$$p"
    }.mkString

    val path = segments.map {
      case s: Segment.Simple => segmentPath(List(s))
      case Segment.Mixed(ps) => segmentPath(ps)
    }.mkString("/")

    val encoder = e.requestBody.consumes match {
      case Consumes.Empty => Nil
      case Consumes.Entity(n, _) =>
        List(s"_hdrs = $n.headers.headers.map(h => h.name.toString -> h.value) ++ _hdrs")
      case Consumes.JsonBody(n, t) =>
        val hdrs =
          if (e.requestBody.required) List(
            "_hdrs = _encoder.headers.headers.map(h => h.name.toString -> h.value) ++ _hdrs"
          )
          else List(
            s"$n foreach (_ => ",
            "  _hdrs = _encoder.headers.headers.map(h => h.name.toString -> h.value) ++ _hdrs",
            ")"
          )

        s"val _encoder = Helpers.circeEntityEncoder[F, ${typeStr(t)}]" :: hdrs
      case Consumes.FormData(flds) =>
        val (requiredFormData, optFormData) = flds.partition(_._2.required)
        val requiredFormParams = requiredFormData.map {
          case (n, Field(t, rn, _)) => s""""$rn" -> $n${addToString(t)}"""
        }.mkString(", ")
        val optFormParams = optFormData.map {
          case (n, Field(t, rn, _)) =>
            s"""$n foreach (x => _formData = ("$rn", x${addToString(t)}) :: _formData)"""
        }

        List(
          s"var _formData = List[(String, String)]($requiredFormParams)",
          "val _encoder = http4s.UrlForm.entityEncoder[F]",
          "_hdrs = _encoder.headers.headers.map(h => h.name.toString -> h.value) ++ _hdrs"
        ) ++ optFormParams
    }
    val entity = {
      val entityLen = "_entity.length.foreach(l =>" +
        " _hdrs = (\"Content-Length\", l.toString) :: _hdrs)"
      e.requestBody.consumes match {
        case Consumes.Empty => Nil
        case Consumes.FormData(_) => List(
          "val _entity = _encoder.toEntity(http4s.UrlForm(_formData: _*))",
          entityLen
        )
        case Consumes.JsonBody(n, _) if !e.requestBody.required => List(
          s"val _entity = $n.fold[http4s.Entity[F]](http4s.Entity.empty)(_encoder.toEntity)",
          entityLen
        )
        case Consumes.JsonBody(n, _) => List(
          s"val _entity = _encoder.toEntity($n)",
          entityLen
        )
        case Consumes.Entity(_, _) => Nil
      }
    }
    val bodyStr = e.requestBody.consumes match {
      case Consumes.Empty => Nil
      case Consumes.Entity(n, _) => List(s"body = $n.body,")
      case Consumes.JsonBody(_, _) => List("body = _entity.body,")
      case Consumes.FormData(_) => List("body = _entity.body,")
    }

    def runOn(rs: List[(String, Option[(MediaType, Type)])]): List[String] = {
      def decoder(mt: MediaType, t: Type): Option[String] = t match {
        case _ if MediaType.application.json.satisfiedBy(mt) =>
          Some("Helpers.circeEntityDecoder")
        case TString if MediaRange.`text/*`.satisfiedBy(mt) =>
          Some("http4s.EntityDecoder.text[F]")
        case _ => None
      }

      def one(s: String, t: Option[(MediaType, Type)]): String = t match {
        case Some((mt, tp)) => decoder(mt, tp) match {
          case None if rs.length == 1 => s"case Status.$s => F.pure[Media[F]](r)"
          case Some(d) if rs.length == 1 =>
            s"case Status.$s => r.as[${typeStr(t.map(_._2))}](F, $d)"
          case None =>
            val p = producesPlain(e.produces)
            s"case Status.$s => F.pure(Coproduct[$p]($s[Media[F]](r)))"
          case Some(d) => List(
            s"case Status.$s => F.map(r.as[${typeStr(t.map(_._2))}](F, $d))",
            s"(x => Coproduct[${producesPlain(e.produces)}]($s(x)))"
          ).mkString
        }
        case None if rs.length == 1 => s"case Status.$s => F.unit"
        case None => s"case Status.$s => F.pure(Coproduct[${producesPlain(e.produces)}]($s()))"
      }

      List(
        List(s"client.run(_request).use[${producesPlain(e.produces)}](r => r.status match {"),
        rs.map { case (s, t) => "  " + one(s, t) },
        List("  case _ => _onError(_request, r)"),
        List("})")
      ).flatten
    }

    val run = e.produces match {
      case Produces.Untyped => List("client.run(_request)")
      case Produces.One(status, t) => runOn(List(status -> t))
      case Produces.Many(rs) => runOn(rs.toList)
    }

    List(
      List(clientServerApi(e) + " = {"),
      if (queryRequired) List(
        s"  var _query = Vector[(String, Option[String])]($requiredQueryParams)"
      ) else Nil,
      List(s"  var _hdrs = List[(String, String)]($requiredHdrParams)"),
      encoder.map("  " + _),
      params.map("  " + _),
      entity.map("  " + _),
      List(
        List("val _request = Request[F]("),
        List(s"  method = Method.${method.toString.toUpperCase},"),
        List(s"  uri = http4s.Uri("),
        if (queryRequired) List("    query = http4s.Query.fromVector(_query),") else Nil,
        List(
          s"""path = http4s.Uri.Path.unsafeFromString(s"/$path"),""",
          "scheme = scheme,",
          "authority = authority"
        ).map("    " + _),
        List("  ),"),
        bodyStr.map("  " + _),
        List("  headers = http4s.Headers(_hdrs)"),
        List(")")
      ).flatten.map("  " + _),
      run.map("  " + _),
      List("}")
    ).flatten
  }

  def apply(pkg: String, endpoints: Map[List[Segment], Map[Method, Endpoint]]): String = {
    List(
      s"package $pkg",
      "",
      "import api4s.internal.Helpers",
      "import api4s.outputs._",
      "import cats.effect.{ Async, Resource }",
      "import io.circe.Json",
      "import org.http4s.client.{ Client, UnexpectedStatus }",
      "import org.http4s.{ Media, Method, Request, Response, Status, Uri }",
      "import org.http4s",
      "import shapeless.{ :+:, CNil, Coproduct }",
      "",
      s"import $pkg.Model._",
      "",
      "class Http4sClient[F[_]](",
      "  client: Client[F],",
      "  scheme: Option[Uri.Scheme] = None,",
      "  authority: Option[Uri.Authority] = None,",
      "  onError: Option[Response[F] => F[Throwable]] = None",
      ")(implicit F: Async[F]) extends Api[F, F] {",
      "  private[this] def _onError[A](req: Request[F], res: Response[F]) = onError.fold[F[A]](",
      "    F.raiseError(UnexpectedStatus(res.status, req.method, req.uri))",
      "  )(f => F.flatMap(f(res))(F.raiseError))",
      "",
      endpoints.flatMap { case (segments, eps) =>
        eps.map { case (method, e) =>
          endpoint(segments, method, e).map("  " + _).mkString("\n")
        }
      }.mkString("\n\n"),
      "}"
    ).mkString("\n")
  }
}
