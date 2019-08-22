package example.petstore

import cats.effect.{ ExitCode, IO, IOApp }
import example.petstore.Server.PetNotFound
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{ Response, Status }

object Main extends IOApp {
  private[this] val httpApp = new Http4sServer[IO](new Server).toHttpApp

  def run(args: List[String]): IO[ExitCode] = for {
    _ <- BlazeServerBuilder[IO]
      .bindHttp(8080)
      .withHttpApp(httpApp)
      .withServiceErrorHandler(r => {
        case _: PetNotFound => IO.pure(Response[IO](status = Status.NotFound))
      })
      .resource
      .use(_ => IO.never)
  } yield ExitCode.Success
}
