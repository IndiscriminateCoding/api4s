package api4s

import cats.data.Validated._
import cats.data._
import io.circe.Json
import org.http4s._
import org.typelevel.ci.CIString

import scala.util.control.NonFatal

trait Decode[+A] { self =>
  def apply(in: String, name: String): ValidatedNec[Throwable, A]
  def apply(in: UrlForm, name: String): ValidatedNec[Throwable, A]
  def apply(in: Query, name: String): ValidatedNec[Throwable, A]
  def apply(in: Headers, name: String): ValidatedNec[Throwable, A]

  final def map[B](f: A => B): Decode[B] = new Decode[B] {
    def apply(in: String, name: String): ValidatedNec[Throwable, B] = self(in, name).map(f)
    def apply(in: UrlForm, name: String): ValidatedNec[Throwable, B] = self(in, name).map(f)
    def apply(in: Query, name: String): ValidatedNec[Throwable, B] = self(in, name).map(f)
    def apply(in: Headers, name: String): ValidatedNec[Throwable, B] = self(in, name).map(f)
  }
}

object Decode {
  def apply[A](implicit decode: Decode[A]): Decode[A] = decode

  private[this] def moreThanOneParameters[A](
    where: String,
    name: String,
    xs: List[A]
  ): ValidatedNec[ParseFailure, Nothing] = {
    val sanitized = s"${xs.length} $where parameter(s) with name=$name present"
    val details = xs.mkString("[", ",", "]")
    Validated.invalidNec(ParseFailure(sanitized, details))
  }

  implicit def decodeExactlyOne[A](implicit decode: Decode[List[A]]): Decode[A] =
    new Decode[A] {
      private[this] def exactlyOne(
        where: String,
        name: String
      )(x: ValidatedNec[Throwable, List[A]]): ValidatedNec[Throwable, A] = x.fold(Invalid(_), {
        case x :: Nil => Valid(x)
        case Nil =>
          Validated.invalidNec(ParseFailure(s"$where parameter (name=$name) not found", ""))
        case xs => moreThanOneParameters(where, name, xs)
      })

      def apply(in: String, name: String): ValidatedNec[Throwable, A] =
        exactlyOne("path", name)(decode(in, name))
      def apply(in: UrlForm, name: String): ValidatedNec[Throwable, A] =
        exactlyOne("form", name)(decode(in, name))
      def apply(in: Query, name: String): ValidatedNec[Throwable, A] =
        exactlyOne("query", name)(decode(in, name))
      def apply(in: Headers, name: String): ValidatedNec[Throwable, A] =
        exactlyOne("header", name)(decode(in, name))
    }

  implicit def decodeOption[A](implicit decode: Decode[List[A]]): Decode[Option[A]] =
    new Decode[Option[A]] {
      private[this] def option(
        where: String,
        name: String
      )(x: ValidatedNec[Throwable, List[A]]): ValidatedNec[Throwable, Option[A]] =
        x.fold(Invalid(_), {
          case Nil => Valid(None)
          case x :: Nil => Valid(Some(x))
          case xs => moreThanOneParameters(where, name, xs)
        })

      def apply(in: String, name: String): ValidatedNec[Throwable, Option[A]] =
        option("path", name)(decode(in, name))
      def apply(in: UrlForm, name: String): ValidatedNec[Throwable, Option[A]] =
        option("form", name)(decode(in, name))
      def apply(in: Query, name: String): ValidatedNec[Throwable, Option[A]] =
        option("query", name)(decode(in, name))
      def apply(in: Headers, name: String): ValidatedNec[Throwable, Option[A]] =
        option("header", name)(decode(in, name))
    }

  implicit val decodeListString: Decode[List[String]] = fromCasts(
    Uri.decode(_),
    identity,
    identity,
    identity
  )("string")

  implicit val decodeListInt: Decode[List[Int]] = fromCast(_.toInt)("int")

  implicit val decodeListLong: Decode[List[Long]] = fromCast(_.toLong)("long")

  implicit val decodeListFloat: Decode[List[Float]] = fromCast(_.toFloat)("float")

  implicit val decodeListDouble: Decode[List[Double]] = fromCast(_.toDouble)("double")

  implicit val decodeListBoolean: Decode[List[Boolean]] = fromCast(_.toBoolean)("boolean")

  def fromCast[A](cast: String => A)(typeName: String): Decode[List[A]] =
    fromCasts(cast, cast, cast, cast)(typeName)

  def fromCasts[A](
    path: String => A,
    form: String => A,
    query: String => A,
    header: String => A
  )(typeName: String): Decode[List[A]] = new Decode[List[A]] {

    import cats.implicits._

    private[this] def tryCast(
      where: String,
      name: String
    )(cast: => A): ValidatedNec[Throwable, A] =
      try Valid(cast)
      catch {
        case NonFatal(_) => Validated.invalidNec(ParseFailure(
          sanitized = s"can't convert $where parameter (name=$name) to $typeName",
          details = s"value=${Json.fromString(name)}"
        ))
      }

    def apply(in: String, name: String): ValidatedNec[Throwable, List[A]] =
      tryCast("path", name)(path(in)).map(_ :: Nil)

    def apply(in: UrlForm, name: String): ValidatedNec[Throwable, List[A]] =
      in.get(name).traverse(x => tryCast("form", name)(form(x))).map(_.toList)

    def apply(in: Query, name: String): ValidatedNec[Throwable, List[A]] =
      in.pairs.foldLeft[List[String]](Nil) {
        case (acc, (n, Some(v))) if n == name => v :: acc
        case (acc, _) => acc
      }.traverse(x => tryCast("query", name)(query(x)))

    def apply(in: Headers, name: String): ValidatedNec[Throwable, List[A]] =
      in.headers.foldLeft[List[String]](Nil) {
        case (acc, h) if h.name == CIString(name) => h.value :: acc
        case (acc, _) => acc
      }.traverse(x => tryCast("header", name)(header(x)))
  }
}
