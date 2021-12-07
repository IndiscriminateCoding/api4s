package api4s

import api4s.Endpoint._
import cats.Applicative
import cats.data.Kleisli
import org.http4s._

trait Endpoint[F[_]] { self =>
  def apply(r: Request[F])(R: RoutingErrorAlgebra[F]): F[Response[F]]

  final def orElse(other: Endpoint[F]): Endpoint[F] = new Endpoint[F] {
    def apply(request: Request[F])(R: RoutingErrorAlgebra[F]): F[Response[F]] =
      self(request)(new RoutingErrorAlgebra[F] {
        def methodNotAllowed(a: Set[Method]): F[Response[F]] =
          other(request)(new RoutingErrorAlgebra[F] {
            def methodNotAllowed(b: Set[Method]): F[Response[F]] = R.methodNotAllowed(a ++ b)

            def notFound: F[Response[F]] = R.methodNotAllowed(a)
          })

        def notFound: F[Response[F]] = other(request)(R)
      })
  }

  final def toHttpApp(implicit F: Applicative[F]): HttpApp[F] = Kleisli(run)

  final def run(r: Request[F])(implicit F: Applicative[F]): F[Response[F]] =
    apply(r)(new RoutingErrorAlgebra[F] {
      def methodNotAllowed(allowed: Set[Method]): F[Response[F]] = F.pure(Response(
        headers = Headers("Allow" -> allowed.mkString(", ")),
        status = Status.MethodNotAllowed
      ))

      def notFound: F[Response[F]] = F.pure(Response(status = Status.NotFound))
    })
}

object Endpoint {
  trait RoutingErrorAlgebra[F[_]] {
    def methodNotAllowed(allowed: Set[Method]): F[Response[F]]

    def notFound: F[Response[F]]

    final def methodNotAllowed(allowed: Method*): F[Response[F]] = methodNotAllowed(allowed.toSet)
  }
}
