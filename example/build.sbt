ThisBuild / organization := "com.github.IndiscriminateCoding"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.2.0"
val http4sVersion = "0.21.0-M5"

lazy val example = (project in file("."))
  .enablePlugins(Api4s)
  .settings(
    scalacOptions += "-language:higherKinds",
    api4sSources := Seq(Api4s.Src(
      file = sourceDirectory.value / "main" / "swagger" / "petstore.yaml",
      pkg = "example.petstore",
      server = true,
      client = true
    ).without4xx),
    name := "example",
    libraryDependencies ++= Seq(
      organization.value %% "api4s-core" % version.value,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-client" % http4sVersion,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "ch.qos.logback" % "logback-classic" % "1.2.3",

      "org.http4s" %% "http4s-blaze-client" % http4sVersion % "test",
      "org.scalatest" %% "scalatest" % "3.0.8" % "test"
    )
  )
