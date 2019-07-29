ThisBuild / organization := "com.github.IndiscriminateCoding"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.0.2"

lazy val example = (project in file("."))
  .enablePlugins(Api4s)
  .settings(
    api4sSources := Seq(Api4s.Src(
      file = sourceDirectory.value / "main" / "swagger" / "petstore.yaml",
      pkg = "example.petstore",
      server = true,
      client = true
    )),
    name := "example",
    libraryDependencies ++= Seq(
      organization.value %% "api4s-runtime" % version.value,
      "org.http4s" %% "http4s-blaze-server" % "0.21.0-M2"
    )
  )
