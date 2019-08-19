package api4s

import cats.data.Validated._
import cats.data._
import io.circe.Json
import org.http4s._

import scala.util.control.NonFatal

trait Decode[-I, +A] {
  def apply(in: I, name: String): ValidatedNec[Throwable, A]

  final def contramap[B](f: B => I): Decode[B, A] = (in, name) => apply(f(in), name)

  final def map[B](f: A => B): Decode[I, B] = (in, name) => apply(in, name).map(f)
}

object Decode {
  def apply[A]: DecodePartiallyApplied[A] = new DecodePartiallyApplied[A]

  class DecodePartiallyApplied[A](private val _unused: Unit = ()) extends AnyVal {
    def apply[In](in: In, name: String)(implicit
      decode: Decode[In, A]
    ): ValidatedNec[Throwable, A] = decode(in, name)
  }

  final class Location[T](val get: String) extends AnyVal {
    override def toString: String = get
  }
  object Location {
    implicit val urlFormLocation: Location[UrlForm] = new Location("form")
    implicit val queryLocation: Location[Query] = new Location("query")
    implicit val headerLocation: Location[Headers] = new Location("header")
  }

  implicit def decodeAtLeastOne[I, A](implicit
    D: Decode[I, List[A]],
    L: Location[I]
  ): Decode[I, A] = (in, name) => D(in, name).fold(Invalid(_), {
    case x :: _ => Valid(x)
    case Nil => Validated.invalidNec(ParseFailure(s"$L parameter (name=$name) not found", ""))
  })

  implicit def decodeOption[I, A](implicit D: Decode[I, List[A]]): Decode[I, Option[A]] =
    D.map(_.headOption)

  implicit val decodeStringFromPath: Decode[String, String] =
    decodePathFromCast(identity, "String")
  implicit val decodeListStringFromUrlForm: Decode[UrlForm, List[String]] =
    decodeListFromUrlForm(identity, "String")
  implicit val decodeListStringFromQuery: Decode[Query, List[String]] =
    decodeListFromQuery(identity, "String")
  implicit val decodeListStringFromHeaders: Decode[Headers, List[String]] =
    decodeListFromHeaders(identity, "String")

  implicit val decodeIntFromPath: Decode[String, Int] =
    decodePathFromCast(_.toInt, "Int")
  implicit val decodeListIntFromUrlForm: Decode[UrlForm, List[Int]] =
    decodeListFromUrlForm(_.toInt, "Int")
  implicit val decodeListIntFromQuery: Decode[Query, List[Int]] =
    decodeListFromQuery(_.toInt, "Int")
  implicit val decodeListIntFromHeaders: Decode[Headers, List[Int]] =
    decodeListFromHeaders(_.toInt, "Int")

  implicit val decodeLongFromPath: Decode[String, Long] =
    decodePathFromCast(_.toLong, "Long")
  implicit val decodeListLongFromUrlForm: Decode[UrlForm, List[Long]] =
    decodeListFromUrlForm(_.toLong, "Long")
  implicit val decodeListLongFromQuery: Decode[Query, List[Long]] =
    decodeListFromQuery(_.toLong, "Long")
  implicit val decodeListLongFromHeaders: Decode[Headers, List[Long]] =
    decodeListFromHeaders(_.toLong, "Long")

  implicit val decodeFloatFromPath: Decode[String, Float] =
    decodePathFromCast(_.toFloat, "Float")
  implicit val decodeListFloatFromUrlForm: Decode[UrlForm, List[Float]] =
    decodeListFromUrlForm(_.toFloat, "Float")
  implicit val decodeListFloatFromQuery: Decode[Query, List[Float]] =
    decodeListFromQuery(_.toFloat, "Float")
  implicit val decodeListFloatFromHeaders: Decode[Headers, List[Float]] =
    decodeListFromHeaders(_.toFloat, "Float")

  implicit val decodeDoubleFromPath: Decode[String, Double] =
    decodePathFromCast(_.toDouble, "Double")
  implicit val decodeListDoubleFromUrlForm: Decode[UrlForm, List[Double]] =
    decodeListFromUrlForm(_.toDouble, "Double")
  implicit val decodeListDoubleFromQuery: Decode[Query, List[Double]] =
    decodeListFromQuery(_.toDouble, "Double")
  implicit val decodeListDoubleFromHeaders: Decode[Headers, List[Double]] =
    decodeListFromHeaders(_.toDouble, "Double")

  implicit val decodeBooleanFromPath: Decode[String, Boolean] =
    decodePathFromCast(_.toBoolean, "Boolean")
  implicit val decodeListBooleanFromUrlForm: Decode[UrlForm, List[Boolean]] =
    decodeListFromUrlForm(_.toBoolean, "Boolean")
  implicit val decodeListBooleanFromQuery: Decode[Query, List[Boolean]] =
    decodeListFromQuery(_.toBoolean, "Boolean")
  implicit val decodeListBooleanFromHeaders: Decode[Headers, List[Boolean]] =
    decodeListFromHeaders(_.toBoolean, "Boolean")

  def decodePathFromCast[A](cast: String => A, typeName: String): Decode[String, A] =
    (in, name) =>
      try Valid(cast(in))
      catch {
        case NonFatal(_) => Validated.invalidNec(ParseFailure(
          sanitized = s"can't convert path parameter (name=$name) to $typeName",
          details = s"value=${Json.fromString(in)}"
        ))
      }

  def decodeListFromCastAndGet[I, A](
    cast: String => A,
    typeName: String,
    get: (I, String) => List[String]
  )(implicit L: Location[I]): Decode[I, List[A]] = (in, name) => {
    import cats.instances.list._
    import cats.syntax.traverse._

    get(in, name).traverse(s =>
      try Valid(cast(s))
      catch {
        case NonFatal(_) => Validated.invalidNec(ParseFailure(
          sanitized = s"can't convert $L parameter (name=$name) to $typeName",
          details = s"value=${Json.fromString(s)}"
        ))
      }
    )
  }

  def decodeListFromUrlForm[A](cast: String => A, typeName: String): Decode[UrlForm, List[A]] =
    decodeListFromCastAndGet(cast, typeName, (f, n) => f.get(n).toList)

  def decodeListFromQuery[A](cast: String => A, typeName: String): Decode[Query, List[A]] =
    decodeListFromCastAndGet(cast, typeName, (f, n) => f.pairs.foldLeft[List[String]](Nil) {
      case (acc, (name, Some(v))) if name == n => v :: acc
      case (acc, _) => acc
    })

  def decodeListFromHeaders[A](cast: String => A, typeName: String): Decode[Headers, List[A]] =
    decodeListFromCastAndGet(cast, typeName, (f, n) => f.toList.foldLeft[List[String]](Nil) {
      case (acc, h) if h.name.value.equalsIgnoreCase(n) => h.value :: acc
      case (acc, _) => acc
    })
}
