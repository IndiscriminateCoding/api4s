package api4s.internal

import cats.data.Validated._
import cats.data.{ Validated, ValidatedNec }
import cats.effect._
import cats.implicits._
import cats.{ Functor, MonadThrow }
import fs2.{ Chunk, Pure, Stream }
import io.circe._
import org.http4s._
import org.http4s.circe._

object Runtime {
  def decode[F[_] : MonadThrow, A](m: Media[F])(implicit decoder: EntityDecoder[F, A]): F[A] =
    decoder.decode(m, strict = true).value.rethrow

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

  def validated[F[_] : Functor, A](
    implicit decoder: EntityDecoder[F, A]
  ): EntityDecoder[F, ValidatedNec[Throwable, A]] =
    decoder.transform {
      case Left(e) => Right(Validated.invalidNec(e))
      case Right(a) => Right(Valid(a))
    }

  def option[F[_] : Concurrent, A](
    implicit decoder: EntityDecoder[F, A]
  ): EntityDecoder[F, Option[A]] =
    new EntityDecoder[F, Option[A]] {
      def decode(m: Media[F], strict: Boolean): DecodeResult[F, Option[A]] =
        if (m.contentType.nonEmpty) decoder.decode(m, strict).map(Some(_))
        else DecodeResult.success(m.body.compile.drain.as(None))

      def consumes: Set[MediaRange] = decoder.consumes
    }


  val printer: Printer = Printer.spaces2.copy(dropNullValues = true)

  def jsonDecoder[F[_] : Concurrent, A : Decoder]: EntityDecoder[F, A] = jsonOf[F, A]

  def textEncoder[F[_]](mediaType: String): EntityEncoder[F, String] =
    EntityEncoder.simple[F, String]("Content-Type" -> mediaType)(s =>
      Chunk.array(s.getBytes(Charset.`UTF-8`.nioCharset))
    )
}
