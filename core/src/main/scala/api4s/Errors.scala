package api4s

import cats.data.{ Chain, NonEmptyChain }

import scala.util.control.NoStackTrace

class Errors private(
  a: Throwable,
  b: Throwable,
  z: Chain[Throwable]
) extends Exception(s"${z.length + 2} errors total") with NoStackTrace {
  def errors: NonEmptyChain[Throwable] = NonEmptyChain.fromChainPrepend(a, b +: z)
}

object Errors {
  def apply(errors: NonEmptyChain[Throwable]): Throwable = {
    def flatten(t: Throwable) = t match {
      case e: Errors => e.errors
      case _ => NonEmptyChain(t)
    }

    val flattened = errors.reduceLeftTo(flatten) {
      case (a, e: Errors) => a ++ e.errors
      case (a, t) => t +: a
    }

    val (a, t) = flattened.uncons
    t.uncons match {
      case None => a
      case Some((b, z)) => new Errors(a, b, z)
    }
  }
}
