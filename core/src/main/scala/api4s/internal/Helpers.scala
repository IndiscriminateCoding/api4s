package api4s.internal

import api4s.Media
import cats.Applicative
import cats.data.Chain
import cats.effect.Sync
import fs2.Chunk
import io.circe.{ Decoder, Encoder, Printer }
import org.http4s._
import org.http4s.circe._
import org.http4s.util.CaseInsensitiveString

import scala.util.control.NonFatal

object Helpers {
  object RequestValidationError extends Exception

  sealed trait Parser[A] {
    def apply(s: String): A

    final def required(s: => String): A = try {
      apply(s)
    } catch {
      case NonFatal(_) => throw RequestValidationError
    }

    final def optional(s: => String): Option[A] = try {
      Some(apply(s))
    } catch {
      case NonFatal(_) => None
    }
  }
  def parser[A](implicit P: Parser[A]): Parser[A] = P

  implicit val stringParser: Parser[String] = new Parser[String] {
    def apply(s: String): String = s
  }
  implicit val intParser: Parser[Int] = new Parser[Int] {
    def apply(s: String): Int = s.toInt
  }
  implicit val longParser: Parser[Long] = new Parser[Long] {
    def apply(s: String): Long = s.toLong
  }
  implicit val booleanParser: Parser[Boolean] = new Parser[Boolean] {
    def apply(s: String): Boolean = s.toBoolean
  }
  implicit val floatParser: Parser[Float] = new Parser[Float] {
    def apply(s: String): Float = s.toFloat
  }
  implicit val doubleParser: Parser[Double] = new Parser[Double] {
    def apply(s: String): Double = s.toDouble
  }

  implicit class RichRequest[F[_]](val r: Request[F]) extends AnyVal {
    def pathSegments: List[String] = r.uri.path.split('/').filter(_.nonEmpty).toList

    /* Headers */
    private[this] def getHeader(n: String): String = r.headers
      .get(CaseInsensitiveString(n))
      .get
      .value

    def header[A](n: String)(implicit P: Parser[A]): A = P.required(getHeader(n))

    def headerOpt[A](n: String)(implicit P: Parser[A]): Option[A] = P.optional(getHeader(n))

    /* Query parameters */
    private[this] def getQuery(n: String): List[String] = r.uri.query
      .multiParams
      .getOrElse(n, Seq.empty)
      .toList

    def query[A](n: String)(implicit P: Parser[A]): A = getQuery(n) match {
      case List(x) => P.required(x)
      case _ => throw RequestValidationError
    }

    def queryOpt[A](n: String)(implicit P: Parser[A]): Option[A] = getQuery(n) match {
      case Nil => None
      case List(x) => Some(P.required(x))
      case _ => throw RequestValidationError
    }

    def queries[A](n: String)(implicit P: Parser[A]): List[A] =
      getQuery(n).map(p => P.required(p))

    def decodeJsonOpt[A: Decoder](f: Option[A] => F[Response[F]])(
      implicit F: Sync[F]
    ): F[Response[F]] = headerOpt[String]("Content-Type") match {
      case None => f(None)
      case Some(_) => r.decode[A](a => f(Some(a)))(F, circeEntityDecoder[F, A])
    }
  }

  implicit class RichUrlForm(val f: Map[String, Chain[String]]) extends AnyVal {
    private[this] def getParam(n: String): List[String] =
      f.getOrElse(n, Chain.empty)
        .toList

    def param[A](n: String)(implicit P: Parser[A]): A = getParam(n) match {
      case List(x) => P.required(x)
      case _ => throw RequestValidationError
    }

    def paramOpt[A](n: String)(implicit P: Parser[A]): Option[A] = getParam(n) match {
      case Nil => None
      case List(x) => Some(P.required(x))
      case _ => throw RequestValidationError
    }

    def params[A](n: String)(implicit P: Parser[A]): List[A] =
      getParam(n).map(p => P.required(p))
  }

  private val printer = Printer.spaces2.copy(dropNullValues = true)

  def circeEntityEncoder[F[_] : Applicative, A: Encoder]: EntityEncoder[F, A] =
    jsonEncoderWithPrinterOf[F, A](printer)

  def circeEntityDecoder[F[_] : Sync, A: Decoder]: EntityDecoder[F, A] = jsonOf[F, A]

  def jsonResponse[F[_] : Applicative, A: Encoder](status: Status)(a: A): Response[F] = {
    val encoder = circeEntityEncoder[F, A]
    val entity = encoder.toEntity(a)

    Response(
      status = status,
      headers = encoder.headers,
      body = entity.body
    )
  }

  def textResponse[F[_]](status: Status, mediaType: String)(text: String): Response[F] = {
    val encoder = EntityEncoder.simple[F, String](Header("Content-Type", mediaType))(s =>
      Chunk.bytes(s.getBytes(DefaultCharset.nioCharset))
    )

    Response(
      status = status,
      headers = encoder.headers,
      body = encoder.toEntity(text).body
    )
  }

  def mediaResponse[F[_]](status: Status)(media: Media[F]): Response[F] = Response(
    status = status,
    headers = media.headers,
    body = media.body
  )

  def emptyResponse[F[_]](status: Status): Response[F] =
    Response(status = status)
}
