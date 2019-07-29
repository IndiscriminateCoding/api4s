package api4s.sbt

import api4s.codegen.ast.Stages
import api4s.codegen.emitter._
import api4s.codegen.swagger
import sbt.Keys._
import sbt._

import scala.collection.mutable

object Api4s extends AutoPlugin {
  case class Src(file: File, pkg: String, server: Boolean = true, client: Boolean = true)

  object autoImport {
    val api4sSources = settingKey[Seq[Src]]("Sources for api4s codegen")
  }
  import autoImport._

  override lazy val projectSettings: Seq[Def.Setting[_]] = super.projectSettings ++ Seq(
    sourceGenerators in Compile += Def.task {
      def parsePkg(s: String): File =
        s.split('.').foldLeft((sourceManaged in Compile).value) { case (pkg, frg) => pkg / frg }

      api4sSources.value flatMap { case Src(file, pkg, server, client) =>
        val src = scala.io.Source.fromFile(file)
        val api = try {
          Stages(swagger.Root(src.mkString).api)
        } finally {
          src.close()
        }
        val p = parsePkg(pkg)
        val res = mutable.Buffer[File]()

        // Model
        val model = p / "Model.scala"
        res += model
        IO.write(model, CirceModel(pkg, api.types))

        // Api trait
        if (server || client) {
          val apiF = p / "Api.scala"
          res += apiF
          IO.write(apiF, ClientServerApi(pkg, api.endpoints))
        }

        // Server
        if (server) {
          val server = p / "Http4sServer.scala"
          res += server
          IO.write(server, Http4sServer(pkg, api.endpoints))
        }

        // Client
        if (client) {
          val client = p / "Http4sClient.scala"
          res += client
          IO.write(client, Http4sClient(pkg, api.endpoints))
        }
        res
      }
    }.taskValue
  )
}
