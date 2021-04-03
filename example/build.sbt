ThisBuild / organization := "com.github.IndiscriminateCoding"
ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := Versions.api4s

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
      "org.http4s" %% "http4s-blaze-server" % Versions.http4s,
      "org.http4s" %% "http4s-client" % Versions.http4s,
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "ch.qos.logback" % "logback-classic" % "1.2.3",

      "org.http4s" %% "http4s-blaze-client" % Versions.http4s % "test",
      "org.scalatest" %% "scalatest" % "3.2.7" % "test"
    )
  )
