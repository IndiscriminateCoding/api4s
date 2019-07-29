package example.petstore

import cats.effect.{ ExitCode, IO, IOApp }
import org.http4s.server.blaze.BlazeServerBuilder

object Main extends IOApp {
  private[this] val httpApp = new Http4sServer[IO](new Server).toHttpApp

  def run(args: List[String]): IO[ExitCode] = for {
    server <- BlazeServerBuilder[IO]
      .bindHttp(8080)
      .withHttpApp(httpApp)
      .resource
      .use(_ => IO.never)
  } yield ExitCode.Success
}
