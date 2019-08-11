package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._
import org.http4s.MediaType

object Http4sClient {
  private def endpoint(segments: List[Segment], method: Method, e: Endpoint): List[String] = {
    import ParameterType._

    def addToString(t: Type): String = t match {
      case TString => ""
      case TInt | TLong | TBool => ".toString"
      case _ => throw new Exception(s"can't convert type $t to String")
    }

    val params = e.parameters flatMap {
      case (Query(rn), Parameter(n, TArr(t), _)) =>
        List(s"""$n foreach (x => _query += "$rn" -> Some(x${addToString(t)}))""")
      case (Query(rn), Parameter(n, t, false)) =>
        List(s"""$n foreach (x => _query += "$rn" -> Some(x${addToString(t)}))""")
      case (Hdr(rn), Parameter(n, t, false)) =>
        List(s"""$n foreach (x => _headers += http4s.Header("$rn", x${addToString(t)}))""")
      case _ => Nil
    }
    val requiredQueryParams = e.parameters.filter {
      case (Query(_), Parameter(_, TArr(_), _)) => false
      case (Query(_), p) => p.required
      case _ => false
    }.map {
      case (Query(rn), Parameter(n, t, _)) => s""""$rn" -> Some($n${addToString(t)})"""
      case _ => throw new Exception("never happens")
    }.mkString(", ")
    val requiredHdrParams = e.parameters.filter {
      case (Hdr(_), p) => p.required
      case _ => false
    }.map {
      case (Hdr(rn), Parameter(n, t, _)) => s"""htp4s.Header("$rn", $n${addToString(t)})"""
      case _ => throw new Exception("never happens")
    }.mkString(", ")
    val path = segments.map {
      case Segment.Static(s) => s
      case Segment.Argument(p) => s"$$$p"
    }.mkString("/")

    val rbt = RequestBodyType(e.requestBody)

    val encoder = rbt match {
      case RequestBodyType.Empty => Nil
      case RequestBodyType.Raw(_) => Nil
      case RequestBodyType.JsonBody(n, t) =>
        val hdrs =
          if (e.requestBody.required) "_headers ++= _encoder.headers.toList"
          else s"$n foreach (_ => _headers ++= _encoder.headers.toList)"

        List(
        s"val _encoder = Helpers.circeEntityEncoder[F, ${typeStr(t)}]",
          hdrs
      )
      case RequestBodyType.FormData(flds) =>
        val (requiredFormData, optFormData) = flds.partition(_._2.required)
        val requiredFormParams = requiredFormData.map {
          case (n, Field(t, rn, _)) => s""""$rn" -> $n${addToString(t)}"""
        }.mkString(", ")
        val optFormParams = optFormData.map {
          case (n, Field(t, rn, _)) =>
            s"""$n foreach (x => _formData += "$rn" -> x${addToString(t)})"""
        }

        List(
          s"val _formData = mutable.Buffer[(String, String)]($requiredFormParams)",
          "val _encoder = http4s.UrlForm.entityEncoder[F]",
          "_headers ++= _encoder.headers.toList"
        ) ++ optFormParams
    }
    val bodyStr = rbt match {
      case RequestBodyType.Empty => Nil
      case RequestBodyType.Raw(n) => List(s"body = $n, ")
      case RequestBodyType.JsonBody(n, _) if !e.requestBody.required =>
        List(s"body = $n.fold[Stream[F, Byte]](Stream.empty)(_encoder.toEntity(_).body),")
      case RequestBodyType.JsonBody(n, _) => List(s"body = _encoder.toEntity($n).body,")
      case RequestBodyType.FormData(_) =>
        List(s"body = _encoder.toEntity(http4s.UrlForm(_formData: _*)).body,")
    }
    val responseType = ResponseType(e.responses)

    def runOn(rs: List[(String, Option[(MediaType, Type)])]): List[String] = {
      def decoderStr(mt: MediaType): String = () match {
        case _ if isJson(mt) => "Helpers.circeEntityDecoder"
        case _ if isText(mt) => "http4s.EntityDecoder.text[F]"
        case _ => throw new Exception(s"No EntityDecoder for $mt")
      }

      def one(s: String, t: Option[(MediaType, Type)]): String = t match {
        case Some((mt, _)) if rs.length == 1 && isBinary(mt) =>
          s"case Status.$s => F.pure(r.body)"
        case Some((mt, _)) if isBinary(mt) =>
          s"case Status.$s => F.map(r.body)(x => Coproduct[${responseType.plain}]($s(x)))"
        case t @ Some((mt, _)) if rs.length == 1 =>
          s"case Status.$s => r.as[${typeStr(t)}](F, ${decoderStr(mt)})"
        case t @ Some((mt, _)) => List(
          s"case Status.$s => F.map(r.as[${typeStr(t)}](F, ${decoderStr(mt)}))",
          s"(x => Coproduct[${responseType.plain}]($s(x)))"
        ).mkString
        case None if rs.length == 1 => s"case Status.$s => F.unit"
        case None => s"case Status.$s => F.pure(Coproduct[${responseType.plain}]($s()))"
      }

      List(
        List(s"client.fetch[${responseType.plain}](_request)(r => r.status match {"),
        rs.map { case (s, t) => "  " + one(s, t) },
        List("  case s => F.raiseError(UnexpectedStatus(s))"),
        List("})")
      ).flatten
    }

    val run = ResponseType(e.responses) match {
      case ResponseType.Untyped => List(
        "F.map(client.run(_request).allocated){",
        "  case (x, r) => x.withBodyStream(x.body.onFinalize(r))",
        "}"
      )
      case ResponseType.Specific(status, t) => runOn(List(status -> t))
      case ResponseType.Multi(rs) => runOn(rs.toList)
    }

    List(
      List(ClientServerApi(e) + " = {"),
      List(s"  val _query = mutable.Buffer[(String, Option[String])]($requiredQueryParams)"),
      List(s"  val _headers = mutable.Buffer[http4s.Header]($requiredHdrParams)"),
      encoder.map("  " + _),
      params.map("  " + _),
      List(
        List("val _request = Request[F]("),
        List(s"  method = Method.${method.toString.toUpperCase},"),
        List(s"  uri = http4s.Uri("),
        List(s"""    path = s"/$path","""),
        List("    query = http4s.Query(_query: _*)"),
        List("  ),"),
        bodyStr.map("  " + _),
        List("  headers = http4s.Headers.of(_headers: _*)"),
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
      "import api4s.runtime.internal.Helpers",
      "import api4s.runtime.outputs._",
      "import fs2.Stream",
      "import cats.effect.Sync",
      "import cats.effect.Resource",
      "import org.http4s.client.{ Client, UnexpectedStatus }",
      "import org.http4s.{ Method, Request, Response, Status }",
      "import org.http4s",
      "import shapeless.{ :+:, CNil, Coproduct }",
      "",
      "import scala.collection.mutable",
      "",
      s"import $pkg.Model._",
      "",
      "class Http4sClient[F[_]](client: Client[F])(implicit F: Sync[F]) extends Api[F] {",
      endpoints.flatMap { case (segments, eps) =>
        eps.map { case (method, e) =>
          endpoint(segments, method, e).map("  " + _).mkString("\n")
        }
      }.mkString("\n\n"),
      "}"
    ).mkString("\n")
  }
}
