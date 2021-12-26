package api4s.internal

import cats.data.Validated._
import cats.data.{ Validated, ValidatedNec }
import cats.effect._
import cats.implicits._
import cats.{ FlatMap, MonadThrow }
import fs2.{ Chunk, Pure, Stream }
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.headers._

object Runtime {
  def decodeValidatedOpt[F[_], A](r: Request[F])(
    f: ValidatedNec[Throwable, Option[A]] => F[Response[F]]
  )(implicit F: Concurrent[F], D: EntityDecoder[F, A]): F[Response[F]] = {
    val drain = r.body.compile[F, F, Byte].drain
    val none = drain >> f(Valid(None))

    r.headers.get[`Content-Length`] match {
      case Some(l) if l.length == 0 => none
      case _ => r.headers.get[`Content-Type`] match {
        case Some(ct) if D.consumes.exists(ct.mediaType.satisfiedBy) =>
          decodeValidated[F, A](r)(x => f(x.map(Some(_))))
        case Some(ct) =>
          drain >> f(Validated.invalidNec(MediaTypeMismatch(ct.mediaType, D.consumes)))
        case _ => none
      }
    }
  }

  def decodeValidated[F[_], A](r: Request[F])(
    f: ValidatedNec[Throwable, A] => F[Response[F]]
  )(implicit F: FlatMap[F], D: EntityDecoder[F, A]): F[Response[F]] =
    D.decode(r, strict = true).value.flatMap {
      case Left(e) => f(Validated.invalidNec(e))
      case Right(x) => f(Valid(x))
    }

  def decodeOrThrow[F[_], A](r: Request[F])(
    f: A => F[Response[F]]
  )(implicit F: MonadThrow[F], D: EntityDecoder[F, A]): F[Response[F]] =
    D.decode(r, strict = true).rethrowT.flatMap(f)

  def purify[F[_] : Concurrent, A](s: Stream[F, A]): F[Stream[Pure, A]] =
    s.chunks.compile.toVector.map(x => Stream.apply(x: _*).unchunks)

  def purify[F[_] : Concurrent](m: Media[F]): F[Media[Pure]] =
    purify(m.body).map(Media(_, m.headers))

  def purify[F[_] : Concurrent](r: Response[F]): F[Response[Pure]] =
    purify(r.body).map { body =>
      Response[Pure](
        status = r.status,
        httpVersion = r.httpVersion,
        headers = r.headers,
        body = body,
        attributes = r.attributes
      )
    }

  val printer: Printer = Printer.spaces2.copy(dropNullValues = true)

  def jsonDecoder[F[_] : Concurrent, A : Decoder]: EntityDecoder[F, A] = jsonOf[F, A]

  def textEncoder[F[_]](mediaType: String): EntityEncoder[F, String] =
    EntityEncoder.simple[F, String]("Content-Type" -> mediaType)(s =>
      Chunk.array(s.getBytes(Charset.`UTF-8`.nioCharset))
    )
}
