package api4s.runtime

import api4s.runtime.Endpoint._
import cats.Applicative
import cats.data.Kleisli
import org.http4s._

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

  final def toHttpApp(implicit F: Applicative[F]): HttpApp[F] = Kleisli(run)

  final def run(r: Request[F])(implicit F: Applicative[F]): F[Response[F]] =
    apply(r)(new RoutingErrorAlgebra[F] {
      def methodNotAllowed: F[Response[F]] = F.pure(Response(status = Status.MethodNotAllowed))

      def notFound: F[Response[F]] = F.pure(Response(status = Status.NotFound))

      def badRequest: F[Response[F]] = F.pure(Response(status = Status.BadRequest))
    })
}

object Endpoint {
  trait RoutingErrorAlgebra[F[_]] {
    def methodNotAllowed: F[Response[F]]
    def notFound: F[Response[F]]
    def badRequest: F[Response[F]]
  }
}
