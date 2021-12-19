package api4s

import cats.effect.{ MonadCancelThrow, Resource }
import cats.implicits._
import org.http4s._

trait LiftResponse[F[_], S[_]] {
  def liftF[A](req: RequestPrelude, info: RouteInfo)(r: F[A]): S[A]
  def liftResource(
    req: RequestPrelude,
    info: RouteInfo
  )(r: Resource[F, Response[S]]): S[Response[S]]
}

object LiftResponse {
  implicit def identityLiftResponse[F[_] : MonadCancelThrow]: LiftResponse[F, F] =
    new LiftResponse[F, F] {
      def liftF[A](req: RequestPrelude, info: RouteInfo)(r: F[A]): F[A] = r
      def liftResource(
        req: RequestPrelude,
        info: RouteInfo
      )(r: Resource[F, Response[F]]): F[Response[F]] =
        r.allocated.map { case (r, f) => r.withBodyStream(r.body.onFinalizeWeak(f)) }
    }
}