package api4s

import cats.Applicative
import cats.data.NonEmptyChain
import io.circe.Json
import org.http4s._
import org.http4s.circe._

import scala.util.control.NoStackTrace

sealed trait DecodingError extends MessageFailure with NoStackTrace

object DecodingError {
  def apply(sanitized: String, details: String): One = new One(sanitized, details)

  def apply(errors: NonEmptyChain[One]): DecodingError = {
    val (h, t) = errors.uncons
    if (t.uncons.isEmpty) h
    else new Many(errors)
  }

  class One(val sanitized: String, val details: String) extends DecodingError {
    def message: String =
      if (sanitized.isEmpty) details
      else if (details.isEmpty) sanitized
      else s"$sanitized: $details"

    def cause: Option[Throwable] = None

    def toHttpResponse[F[_]](
      httpVersion: HttpVersion
    )(implicit F: Applicative[F]): F[Response[F]] = F.pure(Response[F](
      status = Status.BadRequest,
      httpVersion = httpVersion
    ).withEntity[Json](Json.obj("error" -> Json.fromString(sanitized))))
  }

  class Many(val errors: NonEmptyChain[One]) extends DecodingError {
    def message: String = s"${errors.length} decoding errors"

    def cause: Option[Throwable] = None

    def toHttpResponse[F[_]](
      httpVersion: HttpVersion
    )(implicit F: Applicative[F]): F[Response[F]] = F.pure(Response[F](
      status = Status.BadRequest,
      httpVersion = httpVersion
    ).withEntity[Json](Json.obj(
      "errors" -> Json.arr(errors.iterator.map(e => Json.fromString(e.sanitized)).toSeq: _*)
    )))
  }
}