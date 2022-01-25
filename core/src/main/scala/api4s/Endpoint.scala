package api4s

import api4s.Endpoint._
import cats.Applicative
import cats.data.Kleisli
import org.http4s._

trait Endpoint[F[_]] {
  def apply(r: Request[F], path: List[String], router: Router[F]): F[Response[F]]

  final def orElse(that: Endpoint[F]): Endpoint[F] =
    (request: Request[F], path: List[String], router: Router[F]) =>
      apply(request, path, new Fallback(request, path, router, that))

  final def toHttpApp(implicit F: Applicative[F]): HttpApp[F] = Kleisli(apply)

  final def apply(r: Request[F])(implicit F: Applicative[F]): F[Response[F]] =
    apply(r, new Default[F])

  final def apply(r: Request[F], router: Router[F]): F[Response[F]] =
    apply(r, r.uri.path.segments.iterator.map(_.toString).toList, router)
}

object Endpoint {
  trait Router[F[_]] {
    def route(req: RequestPrelude, info: RouteInfo)(res: F[Response[F]]): F[Response[F]]
    def methodNotAllowed(allowed: Set[Method]): F[Response[F]]
    def notFound: F[Response[F]]
  }

  class Default[F[_]](implicit F: Applicative[F]) extends Router[F] {
    def route(req: RequestPrelude, info: RouteInfo)(res: F[Response[F]]): F[Response[F]] = res

    def methodNotAllowed(allowed: Set[Method]): F[Response[F]] = F.pure(Response(
      headers = Headers("Allow" -> allowed.mkString(", ")),
      status = Status.MethodNotAllowed
    ))

    def notFound: F[Response[F]] = F.pure(Response(status = Status.NotFound))
  }

  private class Fallback[F[_]](
    request: Request[F],
    path: List[String],
    router: Router[F],
    other: Endpoint[F]
  ) extends Router[F] {
    def route(req: RequestPrelude, info: RouteInfo)(res: F[Response[F]]): F[Response[F]] =
      router.route(req, info)(res)

    def methodNotAllowed(a: Set[Method]): F[Response[F]] =
      other(request, path, new Router[F] {
        def route(req: RequestPrelude, info: RouteInfo)(res: F[Response[F]]): F[Response[F]] =
          router.route(req, info)(res)

        def methodNotAllowed(b: Set[Method]): F[Response[F]] = router.methodNotAllowed(a ++ b)

        def notFound: F[Response[F]] = router.methodNotAllowed(a)
      })

    def notFound: F[Response[F]] = other(request, path, router)
  }
}
