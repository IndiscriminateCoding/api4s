package api4s.internal

import api4s.Media
import cats.data.Validated._
import cats.data.{ Validated, ValidatedNec }
import cats.effect.Sync
import cats.{ Applicative, FlatMap }
import fs2.{ Chunk, Stream }
import io.circe.{ Decoder, Encoder, Printer }
import org.http4s._
import org.http4s.circe._
import org.http4s.headers.`Content-Type`

object Helpers {
  implicit class RichRequest[F[_]](val r: Request[F]) extends AnyVal {
    def pathSegments: List[String] = r.uri.path.split('/').filter(_.nonEmpty).toList

    def decodeValidatedOpt[A](
      f: ValidatedNec[Throwable, Option[A]] => F[Response[F]]
    )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
      r.headers.get(`Content-Type`) match {
        case None => f(Valid(None))
        case Some(_) => decodeValidated[A](x => f(x.map(Some(_))))
      }

    def decodeValidated[A](
      f: ValidatedNec[Throwable, A] => F[Response[F]]
    )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
      F.flatMap(D.decode(r, strict = true).value) {
        case Left(e) => f(Validated.invalidNec(e))
        case Right(x) => f(Valid(x))
      }
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
