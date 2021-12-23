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

  implicit def decodeAtLeastOne[A](implicit decode: Decode[List[A]]): Decode[A] =
    new Decode[A] {
      private[this] def atLeastOne(
        where: String,
        name: String
      )(x: ValidatedNec[Throwable, List[A]]): ValidatedNec[Throwable, A] = x.fold(Invalid(_), {
        case x :: _ => Valid(x)
        case Nil =>
          Validated.invalidNec(ParseFailure(s"$where parameter (name=$name) not found", ""))
      })

      def apply(in: String, name: String): ValidatedNec[Throwable, A] =
        atLeastOne("path", name)(decode(in, name))
      def apply(in: UrlForm, name: String): ValidatedNec[Throwable, A] =
        atLeastOne("form", name)(decode(in, name))
      def apply(in: Query, name: String): ValidatedNec[Throwable, A] =
        atLeastOne("query", name)(decode(in, name))
      def apply(in: Headers, name: String): ValidatedNec[Throwable, A] =
        atLeastOne("headers", name)(decode(in, name))
    }

  implicit def decodeOption[A](implicit decode: Decode[List[A]]): Decode[Option[A]] =
    decode.map(_.headOption)

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
    headers: String => A
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
      }.traverse(x => tryCast("headers", name)(headers(x)))
  }
}
