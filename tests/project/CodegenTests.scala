import sbt._

import scala.util.control.NonFatal

object CodegenTests {
  private val specs = List(
//    "twitter" -> "https://raw.githubusercontent.com/APIs-guru/openapi-directory/master/APIs/twitter.com/legacy/1.1/swagger.yaml",
//    "twilio" -> "https://pastebin.com/raw/FxB8BSF9",
    "ringcentral" -> "https://netstorage.ringcentral.com/dpw/api-reference/specs/rc-platform.yml",
    "instagram" -> "https://pastebin.com/raw/SDTguj2M"
  )

  def download(file: File): Seq[(String, File)] =
    specs flatMap {
      case (name, url) =>
        try {
          val to = file / s"$name.yml"
          IO.write(to, IO.readLinesURL(new URL(url)).mkString("\n"))
          List(name -> to)
        } catch {
          case NonFatal(e) =>
            System.err.println(s"Error while downloading '$name': ${e.getMessage}")
            Nil
        }
    }
}
