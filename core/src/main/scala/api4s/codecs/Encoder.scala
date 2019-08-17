package api4s.codecs

trait Encoder[-A] {
  def encode(a: A): String

  final def contramap[B](f: B => A): Encoder[B] = b => encode(f(b))
}

object Encoder {
  def apply[A](implicit e: Encoder[A]): Encoder[A] = e

  implicit val encoderOfString: Encoder[String] = a => a
  implicit val encoderOfInt: Encoder[Int] = _.toString
  implicit val encoderOfLong: Encoder[Long] = _.toString
  implicit val encoderOfBoolean: Encoder[Boolean] = _.toString
}
