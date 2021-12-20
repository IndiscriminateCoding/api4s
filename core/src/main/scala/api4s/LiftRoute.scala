package api4s

import cats.~>
import org.http4s._

trait LiftRoute[F[_], S[_]] {
  def lift[A](req: RequestPrelude, info: RouteInfo)(r: F[A]): S[A]

  final def liftK(req: RequestPrelude, info: RouteInfo): F ~> S =
    new (F ~> S) {
      def apply[A](fa: F[A]): S[A] = lift(req, info)(fa)
    }
}

object LiftRoute {
  implicit def subtypeLiftRoute[S[_], F[a] <: S[a]]: LiftRoute[F, S] =
    new LiftRoute[F, S] {
      def lift[A](req: RequestPrelude, info: RouteInfo)(r: F[A]): S[A] = r
    }
}
