import sbt._

import scala.util.control.NonFatal

object CodegenTests {
  private val specs = List(
    ("twitter", "https://raw.githubusercontent.com/APIs-guru/openapi-directory/master/APIs/twitter.com/legacy/1.1/swagger.yaml", false),
    ("twilio", "https://pastebin.com/raw/FxB8BSF9", false),
    //("ringcentral", "https://netstorage.ringcentral.com/dpw/api-reference/specs/rc-platform.yml", true),
    ("instagram", "https://pastebin.com/raw/SDTguj2M", true)
  )

  def download(file: File): Seq[(String, File, Boolean)] =
    specs flatMap {
      case (name, url, withServer) =>
        try {
          val to = file / s"$name.yml"
          IO.write(to, IO.readLinesURL(new URL(url)).mkString("\n"))
          List((name, to, withServer))
        } catch {
          case NonFatal(e) =>
            System.err.println(s"Error while downloading '$name': ${e.getMessage}")
            Nil
        }
    }
}
