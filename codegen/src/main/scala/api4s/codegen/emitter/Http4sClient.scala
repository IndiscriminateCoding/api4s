package api4s.codegen.emitter

import api4s.codegen.ast.Type._
import api4s.codegen.ast._
import api4s.codegen.emitter.Utils._

object Http4sClient {
  private def endpoint(segments: List[Segment], method: Method, e: Endpoint): List[String] = {
    import ParameterType._

    def addToString(t: Type): String = t match {
      case TString() => ""
      case _ => ".toString"
    }

    val params = e.parameters flatMap {
      case (Query, Parameter(n, rn, TArr(t), _)) =>
        List(s"""$n foreach (x => _query += "$rn" -> Some(x${addToString(t)}))""")
      case (Query, Parameter(n, rn, t, false)) =>
        List(s"""$n foreach (x => _query += "$rn" -> Some(x${addToString(t)}))""")
      case (pt, p) => Nil
    }
    val requiredQueryParams = e.parameters.filter {
      case (Query, Parameter(_, _, TArr(_), _)) => false
      case (pt, p) => pt == Query && p.required
    }.map {
      case (_, Parameter(n, rn, t, _)) => s""""$rn" -> Some($n${addToString(t)})"""
    }.mkString(", ")
    val requiredHdrParams = e.parameters.filter {
      case (pt, p) => pt == Hdr && p.required
    }.map {
      case (_, Parameter(n, rn, t, _)) => s"""Header("$rn", $n${addToString(t)})"""
    }.mkString(", ")
    val path = segments.map {
      case Segment.Static(s) => s
      case Segment.Parameter(p) => s"$$$p"
    }.mkString("/")
    val body = e.parameters.filter { case (pt, _) => pt == Body || pt == FormData }
    val encoder = body match {
      case (Body, Parameter(n, rn, t, true)) :: Nil => List(
        s"val _encoder = Helpers.circeEntityEncoder[F, ${typeStr(t)}]",
        "_headers ++= _encoder.headers.toList"
      )
      case _ => Nil
    }
    val bodyStr = body match {
      case (FormData, Parameter("formData", _, TFile(), _)) :: Nil => List("body = formData,")
      case (Body, Parameter(n, rn, t, true)) :: Nil => List(s"body = _encoder.toEntity($n).body,")
      case Nil => Nil
      case ps => throw new Exception(s"Unexpected Body/FormData parameters in ${e.name}: $ps")
    }
    val responseType = ResponseType(e.responses)

    def runOn(rs: List[(String, Option[Type])]): List[String] = {
      def one(s: String, t: Option[Type]): String = t match {
        case Some(t) if rs.length == 1 =>
          s"case Status.$s => Helpers.decode[F, ${typeStr(t)}](r)"
        case Some(t) => List(
          s"case Status.$s => F.map(Helpers.decode[F, ${typeStr(t)}](r))",
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
      case ResponseType.Untyped => List(s"client.run(_request)")
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
