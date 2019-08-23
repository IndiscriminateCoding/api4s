package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._
import org.http4s.{ MediaRange, MediaType }

object Http4sClient {
  private def endpoint(segments: List[Segment], method: Method, e: Endpoint): List[String] = {
    def addToString(t: Type): String = t match {
      case TString => ""
      case t if primitive(t) => ".toString"
      case _ => throw new Exception(s"can't convert type $t to String")
    }

    val params = e.parameters flatMap {
      case (Parameter.Query(rn), Parameter(n, TArr(t), _)) =>
        List(s"""$n foreach (x => _query += "$rn" -> Some(x${addToString(t)}))""")
      case (Parameter.Query(rn), Parameter(n, t, false)) =>
        List(s"""$n foreach (x => _query += "$rn" -> Some(x${addToString(t)}))""")
      case (Parameter.Hdr(rn), Parameter(n, t, false)) =>
        List(s"""$n foreach (x => _headers += http4s.Header("$rn", x${addToString(t)}))""")
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
    val requiredHdrParams = e.parameters.filter {
      case (Parameter.Hdr(_), p) => p.required
      case _ => false
    }.map {
      case (Parameter.Hdr(rn), Parameter(n, t, _)) =>
        s"""http4s.Header("$rn", $n${addToString(t)})"""
      case _ => throw new Exception("never happens")
    }.mkString(", ")
    val path = segments.map {
      case Segment.Static(s) => s
      case Segment.Argument(p) => s"$$$p"
    }.mkString("/")

    val encoder = e.requestBody.consumes match {
      case Consumes.Empty => Nil
      case Consumes.Entity(n, _) => List(s"_headers ++= $n.headers.iterator")
      case Consumes.JsonBody(n, t) =>
        val hdrs =
          if (e.requestBody.required) "_headers ++= _encoder.headers.toList"
          else s"$n foreach (_ => _headers ++= _encoder.headers.toList)"

        List(
          s"val _encoder = Helpers.circeEntityEncoder[F, ${typeStr(t)}]",
          hdrs
        )
      case Consumes.FormData(flds) =>
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
          "val _encoder = Helpers.urlFormEncoder[F]",
          "_headers ++= _encoder.headers.toList"
        ) ++ optFormParams
    }
    val bodyStr = e.requestBody.consumes match {
      case Consumes.Empty => Nil
      case Consumes.Entity(n, _) => List(s"body = $n.body, ")
      case Consumes.JsonBody(n, _) if !e.requestBody.required =>
        List(s"body = $n.fold[fs2.Stream[F, Byte]](fs2.Stream.empty)(_encoder.toEntity(_).body),")
      case Consumes.JsonBody(n, _) => List(s"body = _encoder.toEntity($n).body,")
      case Consumes.FormData(_) =>
        List(s"body = _encoder.toEntity(http4s.UrlForm(_formData: _*)).body,")
    }

    def runOn(rs: List[(String, Option[(MediaType, Type)])]): List[String] = {
      def decoder(mt: MediaType): Option[String] = () match {
        case _ if MediaType.application.json.satisfiedBy(mt) =>
          Some("Helpers.circeEntityDecoder")
        case _ if MediaRange.`text/*`.satisfiedBy(mt) =>
          Some("http4s.EntityDecoder.text[F]")
        case _ => None
      }

      def one(s: String, t: Option[(MediaType, Type)]): String = t match {
        case Some((mt, _)) => decoder(mt) match {
          case None if rs.length == 1 => s"case Status.$s => F.pure(Media(r))"
          case Some(d) if rs.length == 1 =>
            s"case Status.$s => r.as[${typeStr(t.map(_._2))}](F, $d)"
          case None =>
            val p = producesPlain(e.produces)
            s"case Status.$s => F.pure(Coproduct[$p]($s(Media(r))))"
          case Some(d) => List(
            s"case Status.$s => F.map(r.as[${typeStr(t.map(_._2))}](F, $d))",
            s"(x => Coproduct[${producesPlain(e.produces)}]($s(x)))"
          ).mkString
        }
        case None if rs.length == 1 => s"case Status.$s => F.unit"
        case None => s"case Status.$s => F.pure(Coproduct[${producesPlain(e.produces)}]($s()))"
      }

      List(
        List(s"client.fetch[${producesPlain(e.produces)}](_request)(r => r.status match {"),
        rs.map { case (s, t) => "  " + one(s, t) },
        List("  case s => F.raiseError(UnexpectedStatus(s))"),
        List("})")
      ).flatten
    }

    val run = e.produces match {
      case Produces.Untyped => List("client.run(_request)")
      case Produces.One(status, t) => runOn(List(status -> t))
      case Produces.Many(rs) => runOn(rs.toList)
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
      "import api4s.Media",
      "import api4s.internal.Helpers",
      "import api4s.outputs._",
      "import cats.effect.{ Resource, Sync }",
      "import io.circe.Json",
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
