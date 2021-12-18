package api4s

import api4s.Endpoint._
import cats.ApplicativeThrow
import cats.data.Kleisli
import org.http4s._

trait Endpoint[F[_]] { self =>
  def apply(r: Request[F])(R: Router[F]): F[Response[F]]

  final def orElse(other: Endpoint[F]): Endpoint[F] = new Endpoint[F] {
    def apply(request: Request[F])(R: Router[F]): F[Response[F]] =
      self(request)(new Router[F] {
        def response(info: RouteInfo, res: F[Response[F]]): F[Response[F]] = R.response(info, res)

        def methodNotAllowed(a: Set[Method]): F[Response[F]] =
          other(request)(new Router[F] {
            def response(info: RouteInfo, res: F[Response[F]]): F[Response[F]] =
              R.response(info, res)

            def methodNotAllowed(b: Set[Method]): F[Response[F]] = R.methodNotAllowed(a ++ b)

            def badRequest(info: RouteInfo, t: Throwable): F[Response[F]] = R.badRequest(info, t)

            def notFound: F[Response[F]] = R.methodNotAllowed(a)
          })

        def badRequest(info: RouteInfo, t: Throwable): F[Response[F]] = R.badRequest(info, t)

        def notFound: F[Response[F]] = other(request)(R)
      })
  }

  final def toHttpApp(implicit F: ApplicativeThrow[F]): HttpApp[F] = Kleisli(run)

  final def run(r: Request[F])(implicit F: ApplicativeThrow[F]): F[Response[F]] =
    apply(r)(new Router.Default[F])
}

object Endpoint {
  trait Router[F[_]] {
    def response(info: RouteInfo, res: F[Response[F]]): F[Response[F]]

    def methodNotAllowed(allowed: Set[Method]): F[Response[F]]

    def notFound: F[Response[F]]

    def badRequest(info: RouteInfo, t: Throwable): F[Response[F]]

    final def methodNotAllowed(allowed: Method*): F[Response[F]] = methodNotAllowed(allowed.toSet)
  }

  object Router {
    class Default[F[_]](implicit F: ApplicativeThrow[F]) extends Router[F] {
      def response(info: RouteInfo, res: F[Response[F]]): F[Response[F]] = res

      def methodNotAllowed(allowed: Set[Method]): F[Response[F]] = F.pure(Response(
        headers = Headers("Allow" -> allowed.mkString(", ")),
        status = Status.MethodNotAllowed
      ))

      def badRequest(info: RouteInfo, t: Throwable): F[Response[F]] = F.raiseError(t)

      def notFound: F[Response[F]] = F.pure(Response(status = Status.NotFound))
    }
  }
}
