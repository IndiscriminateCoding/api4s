package api4s.internal

import cats.data.Validated._
import cats.data.{ Validated, ValidatedNec }
import cats.effect.Concurrent
import cats.implicits._
import cats.{ FlatMap, MonadThrow }
import fs2.Chunk
import io.circe.{ Decoder, Encoder, Printer }
import org.http4s._
import org.http4s.circe._
import org.http4s.headers._

object Runtime {
  implicit class RequestOps[F[_]](val r: Request[F]) extends AnyVal {
    def decodeValidatedOpt[A](
      f: ValidatedNec[Throwable, Option[A]] => F[Response[F]]
    )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
      r.headers.get[`Content-Length`] match {
        case Some(l) if l.length == 0 => f(Valid(None))
        case _ => r.headers.get[`Content-Type`] match {
          case Some(ct) if D.consumes.exists(ct.mediaType.satisfiedBy) =>
            decodeValidated[A](x => f(x.map(Some(_))))
          case _ => f(Valid(None))
        }
      }

    def decodeValidated[A](
      f: ValidatedNec[Throwable, A] => F[Response[F]]
    )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
      D.decode(r, strict = true).value.flatMap {
        case Left(e) => f(Validated.invalidNec(e))
        case Right(x) => f(Valid(x))
      }

    def decodeOrThrow[A](
      f: A => F[Response[F]]
    )(implicit F: MonadThrow[F], D: EntityDecoder[F, A]): F[Response[F]] =
      D.decode(r, strict = true).rethrowT.flatMap(f)
  }

  private val printer = Printer.spaces2.copy(dropNullValues = true)

  def circeEntityEncoder[F[_], A : Encoder]: EntityEncoder[F, A] =
    jsonEncoderWithPrinterOf[F, A](printer)

  def circeEntityDecoder[F[_] : Concurrent, A : Decoder]: EntityDecoder[F, A] = jsonOf[F, A]

  def jsonResponse[F[_], A : Encoder](status: Status)(a: A): Response[F] =
    Response(status = status).withEntity(a)(circeEntityEncoder[F, A])

  def textResponse[F[_]](status: Status, mediaType: String)(
    text: String
  ): Response[F] = {
    val encoder = EntityEncoder.simple[F, String]("Content-Type" -> mediaType)(s =>
      Chunk.array(s.getBytes(Charset.`UTF-8`.nioCharset))
    )

    Response(status = status).withEntity(text)(encoder)
  }

  def mediaResponse[F[_]](status: Status)(media: Media[F]): Response[F] = Response(
    status = status,
    headers = media.headers,
    body = media.body
  )

  def emptyResponse[F[_]](status: Status): Response[F] =
    Response(status = status)
}
