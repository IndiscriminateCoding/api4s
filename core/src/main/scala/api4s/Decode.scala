package api4s

import cats.data.Validated._
import cats.data._
import io.circe.Json
import org.http4s.{ Headers, Query, UrlForm }

import scala.util.control.NonFatal

trait Decode[-I, +A] {
  def apply(in: I, name: String): ValidatedNec[DecodingError.One, A]

  final def contramap[B](f: B => I): Decode[B, A] = (in, name) => apply(f(in), name)

  final def map[B](f: A => B): Decode[I, B] = (in, name) => apply(in, name).map(f)
}

object Decode {
  def apply[A]: DecodePartiallyApplied[A] = new DecodePartiallyApplied[A]

  class DecodePartiallyApplied[A](private val _unused: Unit = ()) extends AnyVal {
    def apply[In](in: In, name: String)(implicit
      decode: Decode[In, A]
    ): ValidatedNec[DecodingError.One, A] = decode(in, name)
  }

  implicit def optionDecoder[I, A](implicit d: Decode[I, List[A]]): Decode[I, Option[A]] =
    d.map(_.headOption)

  implicit val pathDecoderForString: Decode[String, String] = StringDecoders.path
  implicit val urlFormDecoderForStringList: Decode[UrlForm, List[String]] =
    StringDecoders.urlForm.many
  implicit val urlFormDecoderForString: Decode[UrlForm, String] =
    StringDecoders.urlForm.one
  implicit val queryDecoderForStringList: Decode[Query, List[String]] =
    StringDecoders.query.many
  implicit val queryDecoderForString: Decode[Query, String] =
    StringDecoders.query.one
  implicit val headerDecoderForStringList: Decode[Headers, List[String]] =
    StringDecoders.headers.many
  implicit val headerDecoderForString: Decode[Headers, String] =
    StringDecoders.headers.one

  implicit val pathDecoderForInt: Decode[String, Int] = IntDecoders.path
  implicit val urlFormDecoderForIntList: Decode[UrlForm, List[Int]] =
    IntDecoders.urlForm.many
  implicit val urlFormDecoderForInt: Decode[UrlForm, Int] =
    IntDecoders.urlForm.one
  implicit val queryDecoderForIntList: Decode[Query, List[Int]] =
    IntDecoders.query.many
  implicit val queryDecoderForInt: Decode[Query, Int] =
    IntDecoders.query.one
  implicit val headerDecoderForIntList: Decode[Headers, List[Int]] =
    IntDecoders.headers.many
  implicit val headerDecoderForInt: Decode[Headers, Int] =
    IntDecoders.headers.one

  implicit val pathDecoderForLong: Decode[String, Long] = LongDecoders.path
  implicit val urlFormDecoderForLongList: Decode[UrlForm, List[Long]] =
    LongDecoders.urlForm.many
  implicit val urlFormDecoderForLong: Decode[UrlForm, Long] =
    LongDecoders.urlForm.one
  implicit val queryDecoderForLongList: Decode[Query, List[Long]] =
    LongDecoders.query.many
  implicit val queryDecoderForLong: Decode[Query, Long] =
    LongDecoders.query.one
  implicit val headerDecoderForLongList: Decode[Headers, List[Long]] =
    LongDecoders.headers.many
  implicit val headerDecoderForLong: Decode[Headers, Long] =
    LongDecoders.headers.one

  implicit val pathDecoderForBoolean: Decode[String, Boolean] = BooleanDecoders.path
  implicit val urlFormDecoderForBooleanList: Decode[UrlForm, List[Boolean]] =
    BooleanDecoders.urlForm.many
  implicit val urlFormDecoderForBoolean: Decode[UrlForm, Boolean] =
    BooleanDecoders.urlForm.one
  implicit val queryDecoderForBooleanList: Decode[Query, List[Boolean]] =
    BooleanDecoders.query.many
  implicit val queryDecoderForBoolean: Decode[Query, Boolean] =
    BooleanDecoders.query.one
  implicit val headerDecoderForBooleanList: Decode[Headers, List[Boolean]] =
    BooleanDecoders.headers.many
  implicit val headerDecoderForBoolean: Decode[Headers, Boolean] =
    BooleanDecoders.headers.one

  private object StringDecoders extends RequestDecoders[String] {
    def cast(s: String): String = s

    def typeName: String = "String"
  }

  private object IntDecoders extends RequestDecoders[Int] {
    def cast(s: String): Int = s.toInt

    def typeName: String = "Int"
  }

  private object LongDecoders extends RequestDecoders[Long] {
    def cast(s: String): Long = s.toLong

    def typeName: String = "Long"
  }

  private object BooleanDecoders extends RequestDecoders[Boolean] {
    def cast(s: String): Boolean = s.toBoolean

    def typeName: String = "Boolean"
  }

  private trait RequestDecoders[A] { self =>
    def cast(s: String): A
    def typeName: String

    def path: Decode[String, A] = (in, name) =>
      try Valid(cast(in))
      catch {
        case NonFatal(_) => Invalid(NonEmptyChain(DecodingError(
          sanitized = s"can't convert path parameter (name=$name) to $typeName",
          details = s"value=${Json.fromString(in)}"
        )))
      }

    def urlForm: Decoders[UrlForm, A] = new Decoders[UrlForm, A] {
      def extract(in: UrlForm, name: String): List[String] = in.get(name).toList

      def cast(s: String): A = self.cast(s)

      def typeName: String = self.typeName

      def location: String = "form"
    }

    def query: Decoders[Query, A] = new Decoders[Query, A] {
      def extract(in: Query, name: String): List[String] = in.pairs.foldLeft[List[String]](Nil) {
        case (acc, (n, Some(v))) if n == name => v :: acc
        case (acc, _) => acc
      }

      def cast(s: String): A = self.cast(s)

      def typeName: String = self.typeName

      def location: String = "query"
    }

    def headers: Decoders[Headers, A] = new Decoders[Headers, A] {
      def extract(in: Headers, name: String): List[String] =
        in.toList.foldLeft[List[String]](Nil) {
          case (acc, h) if h.name.value.equalsIgnoreCase(name) => h.value :: acc
          case (acc, _) => acc
        }

      def cast(s: String): A = self.cast(s)

      def typeName: String = self.typeName

      def location: String = "header"
    }
  }

  private trait Decoders[I, A] {
    def extract(in: I, name: String): List[String]
    def cast(s: String): A
    def typeName: String
    def location: String

    def many: Decode[I, List[A]] = (in, name) => {
      import cats.instances.list._
      import cats.syntax.traverse._

      extract(in, name).traverse(s =>
        try Valid(cast(s))
        catch {
          case NonFatal(_) => Invalid(NonEmptyChain(DecodingError(
            sanitized = s"can't convert $location parameter (name=$name) to $typeName",
            details = s"value=${Json.fromString(s)}"
          )))
        }
      )
    }

    def one: Decode[I, A] = (in, name) => many.apply(in, name).fold(Invalid(_), {
      case x :: _ => Valid(x)
      case Nil => Invalid(NonEmptyChain(
        DecodingError(s"$location parameter (name=$name) not found", "")
      ))
    })
  }
}
