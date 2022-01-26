package api4s

import cats._
import cats.data.{ OptionT, ReaderT }
import org.http4s._

trait Endpoint[F[_]] { self =>
  def apply(req: Request[F], path: List[String]): Endpoint.Result[F]

  final def apply(req: Request[F]): Endpoint.Result[F] =
    apply(req, req.uri.path.segments.iterator.map(_.toString).toList)

  final def run(req: Request[F])(implicit F: Applicative[F]): F[Response[F]] =
    apply(req) match {
      case Endpoint.Route(_, res) => res
      case Endpoint.NotFound => F.pure(Response.notFound[F])
      case Endpoint.MethodNotAllowed(allowed) => F.pure(Response(
        headers = Headers("Allow" -> allowed.mkString(", ")),
        status = Status.MethodNotAllowed
      ))
    }

  final def toHttpApp(implicit F: Applicative[F]): HttpApp[F] = ReaderT(run)

  final def toHttpRoutes(implicit F: Monad[F]): HttpRoutes[F] = HttpRoutes {
    apply(_) match {
      case Endpoint.NotFound => OptionT.none
      case Endpoint.MethodNotAllowed(_) => OptionT.none
      case Endpoint.Route(_, res) => OptionT.liftF(res)
    }
  }

  final def orElse(that: Endpoint[F]): Endpoint[F] = (req: Request[F], path: List[String]) =>
    self(req, path) match {
      case Endpoint.NotFound => that(req, path)
      case res@Endpoint.MethodNotAllowed(a) =>
        that(req, path) match {
          case Endpoint.NotFound => res
          case Endpoint.MethodNotAllowed(b) => Endpoint.MethodNotAllowed(a ++ b)
          case route@Endpoint.Route(_, _) => route
        }
      case route@Endpoint.Route(_, _) => route
    }
}

object Endpoint {
  sealed trait Result[+F[_]]
  case object NotFound extends Result[Nothing]
  case class MethodNotAllowed(allowed: Set[Method]) extends Result[Nothing]
  case class Route[F[_]](info: RouteInfo, res: F[Response[F]]) extends Result[F]
}
