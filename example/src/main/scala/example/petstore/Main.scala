package example.petstore

import cats.effect.{ ExitCode, IO, IOApp }
import example.petstore.Storage.PetNotFound
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s._

object Main extends IOApp {
  val server: Server = new Server(Storage())
  val httpApp: HttpApp[IO] = new Http4sServer[IO, IO](server).toHttpApp

  def run(args: List[String]): IO[ExitCode] = for {
    _ <- BlazeServerBuilder[IO]
      .bindHttp(8080)
      .withHttpApp(httpApp)
      .withServiceErrorHandler(_ => {
        case _: PetNotFound => IO.pure(Response[IO](status = Status.NotFound))
      })
      .resource
      .use(_ => IO.never)
  } yield ExitCode.Success
}
