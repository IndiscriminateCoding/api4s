package api4s.codecs

import cats.data.Validated._
import cats.data._
import io.circe.Json
import org.http4s.{ Headers, Query, UrlForm }

import scala.util.control.NonFatal

trait Decoder[-I, +A] {
  def decode(in: I, name: String): ValidatedNec[DecodingError.One, A]

  final def contramap[B](f: B => I): Decoder[B, A] = (in, name) => decode(f(in), name)

  final def map[B](f: A => B): Decoder[I, B] = (in, name) => decode(in, name).map(f)
}

object Decoder {
  def apply[I, A](implicit d: Decoder[I, A]): Decoder[I, A] = d

  implicit def optionDecoder[I, A](implicit d: Decoder[I, List[A]]): Decoder[I, Option[A]] =
    d.map(_.headOption)

  implicit val pathDecoderForString: Decoder[String, String] = StringDecoders.path
  implicit val urlFormDecoderForStringList: Decoder[UrlForm, List[String]] =
    StringDecoders.urlForm.many
  implicit val urlFormDecoderForString: Decoder[UrlForm, String] =
    StringDecoders.urlForm.one
  implicit val queryDecoderForStringList: Decoder[Query, List[String]] =
    StringDecoders.query.many
  implicit val queryDecoderForString: Decoder[Query, String] =
    StringDecoders.query.one
  implicit val headerDecoderForStringList: Decoder[Headers, List[String]] =
    StringDecoders.headers.many
  implicit val headerDecoderForString: Decoder[Headers, String] =
    StringDecoders.headers.one

  implicit val pathDecoderForInt: Decoder[String, Int] = IntDecoders.path
  implicit val urlFormDecoderForIntList: Decoder[UrlForm, List[Int]] =
    IntDecoders.urlForm.many
  implicit val urlFormDecoderForInt: Decoder[UrlForm, Int] =
    IntDecoders.urlForm.one
  implicit val queryDecoderForIntList: Decoder[Query, List[Int]] =
    IntDecoders.query.many
  implicit val queryDecoderForInt: Decoder[Query, Int] =
    IntDecoders.query.one
  implicit val headerDecoderForIntList: Decoder[Headers, List[Int]] =
    IntDecoders.headers.many
  implicit val headerDecoderForInt: Decoder[Headers, Int] =
    IntDecoders.headers.one

  implicit val pathDecoderForLong: Decoder[String, Long] = LongDecoders.path
  implicit val urlFormDecoderForLongList: Decoder[UrlForm, List[Long]] =
    LongDecoders.urlForm.many
  implicit val urlFormDecoderForLong: Decoder[UrlForm, Long] =
    LongDecoders.urlForm.one
  implicit val queryDecoderForLongList: Decoder[Query, List[Long]] =
    LongDecoders.query.many
  implicit val queryDecoderForLong: Decoder[Query, Long] =
    LongDecoders.query.one
  implicit val headerDecoderForLongList: Decoder[Headers, List[Long]] =
    LongDecoders.headers.many
  implicit val headerDecoderForLong: Decoder[Headers, Long] =
    LongDecoders.headers.one

  implicit val pathDecoderForBoolean: Decoder[String, Boolean] = BooleanDecoders.path
  implicit val urlFormDecoderForBooleanList: Decoder[UrlForm, List[Boolean]] =
    BooleanDecoders.urlForm.many
  implicit val urlFormDecoderForBoolean: Decoder[UrlForm, Boolean] =
    BooleanDecoders.urlForm.one
  implicit val queryDecoderForBooleanList: Decoder[Query, List[Boolean]] =
    BooleanDecoders.query.many
  implicit val queryDecoderForBoolean: Decoder[Query, Boolean] =
    BooleanDecoders.query.one
  implicit val headerDecoderForBooleanList: Decoder[Headers, List[Boolean]] =
    BooleanDecoders.headers.many
  implicit val headerDecoderForBoolean: Decoder[Headers, Boolean] =
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

    def path: Decoder[String, A] = (in, name) =>
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

    def many: Decoder[I, List[A]] = (in, name) => {
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

    def one: Decoder[I, A] = (in, name) => many.decode(in, name).fold(Invalid(_), {
      case x :: _ => Valid(x)
      case Nil => Invalid(NonEmptyChain(
        DecodingError(s"$location parameter (name=$name) not found", "")
      ))
    })
  }
}
