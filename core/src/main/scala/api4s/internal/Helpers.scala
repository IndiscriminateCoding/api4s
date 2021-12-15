package api4s.internal

import cats.FlatMap
import cats.data.Validated._
import cats.data.{ Validated, ValidatedNec }
import cats.effect.Async
import fs2.Chunk
import io.circe.{ Decoder, Encoder, Printer }
import org.http4s._
import org.http4s.circe._
import org.http4s.headers._
import org.typelevel.vault.Vault

object Helpers {
  implicit class RichRequest[F[_]](val r: Request[F]) extends AnyVal {
    def pathSegments: Vector[String] = r.uri.path.segments.map(_.toString).filter(_.nonEmpty)

    def decodeValidatedOpt[A](
      f: ValidatedNec[Throwable, Option[A]] => F[Response[F]]
    )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
      r.headers.get[`Content-Length`] match {
        case Some(l) if l.length == 0 => f(Valid(None))
        case _ => r.headers.get[`Content-Type`] match {
          case None => f(Valid(None))
          case Some(_) => decodeValidated[A](x => f(x.map(Some(_))))
        }
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

  def circeEntityEncoder[F[_], A : Encoder]: EntityEncoder[F, A] =
    jsonEncoderWithPrinterOf[F, A](printer)

  def circeEntityDecoder[F[_] : Async, A : Decoder]: EntityDecoder[F, A] = jsonOf[F, A]

  def jsonResponse[F[_], A : Encoder](status: Status, attrs: Vault)(a: A): Response[F] = {
    val encoder = circeEntityEncoder[F, A]
    val entity = encoder.toEntity(a)

    Response(
      status = status,
      headers = encoder.headers,
      body = entity.body
    )
  }

  def textResponse[F[_]](status: Status, mediaType: String, attrs: Vault)(
    text: String
  ): Response[F] = {
    val encoder = EntityEncoder.simple[F, String]("Content-Type" -> mediaType)(s =>
      Chunk.array(s.getBytes(Charset.`UTF-8`.nioCharset))
    )

    Response(
      status = status,
      headers = encoder.headers,
      body = encoder.toEntity(text).body,
      attributes = attrs
    )
  }

  def mediaResponse[F[_]](status: Status)(media: Media[F], attrs: Vault): Response[F] = Response(
    status = status,
    headers = media.headers,
    body = media.body,
    attributes = attrs
  )

  def emptyResponse[F[_]](status: Status, attrs: Vault): Response[F] =
    Response(status = status, attributes = attrs)
}
