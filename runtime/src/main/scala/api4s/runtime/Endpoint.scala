package api4s.runtime

import api4s.runtime.Endpoint._
import org.http4s.{ Request, Response }

trait Endpoint[F[_]] { self =>
  protected def apply(r: Request[F])(R: RoutingErrorAlgebra[F]): F[Response[F]]

  final def orElse(other: Endpoint[F]): Endpoint[F] = new Endpoint[F] {
    def apply(request: Request[F])(R: RoutingErrorAlgebra[F]): F[Response[F]] =
      self(request)(new RoutingErrorAlgebra[F] {
        def methodNotAllowed: F[Response[F]] = other(request)(new RoutingErrorAlgebra[F] {
          def methodNotAllowed: F[Response[F]] = R.methodNotAllowed

          def notFound: F[Response[F]] = R.methodNotAllowed

          def badRequest: F[Response[F]] = R.badRequest
        })

        def notFound: F[Response[F]] = other(request)(R)

        def badRequest: F[Response[F]] = R.badRequest
      })
  }
}

object Endpoint {
  trait RoutingErrorAlgebra[F[_]] {
    def methodNotAllowed: F[Response[F]]
    def notFound: F[Response[F]]
    def badRequest: F[Response[F]]
  }
}
