lazy val scalaVersions = List("2.12.8" /*, "2.13.0" */)

ThisBuild / version := "0.0.2"
ThisBuild / organization := "com.github.IndiscriminateCoding"
ThisBuild / scalaVersion := "2.12.8"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")

lazy val circeVersion = "0.12.0-M4"
lazy val http4sVersion = "0.21.0-M2"

lazy val api4s = (project in file("."))
  .aggregate(codegen, runtime, `sbt-plugin`)
  .settings(
    publish / skip := true,
    crossScalaVersions := Nil
  )

lazy val codegen = (project in file("codegen"))
  .settings(
    crossScalaVersions := scalaVersions,
    name := "api4s-codegen",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-yaml" % "0.11.0-M1",
      "io.circe" %% "circe-generic" % circeVersion
    )
  )

lazy val runtime = (project in file("runtime"))
  .settings(
    crossScalaVersions := scalaVersions,
    name := "api4s-runtime",
    scalacOptions ++= Seq(
      "-language:higherKinds",
      "-Ypartial-unification"
    ),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-core" % http4sVersion,
      "org.http4s" %% "http4s-client" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "io.circe" %% "circe-generic" % circeVersion
    )
  )

lazy val `sbt-plugin` = (project in file("sbt-plugin"))
  .settings(
    crossScalaVersions := scalaVersions,
    sbtPlugin := true,
    name := "api4s-sbt",
  )
  .dependsOn(codegen)

// sonatype-related settings
ThisBuild / publishTo := sonatypePublishTo.value
ThisBuild / publishMavenStyle := true
ThisBuild / licenses :=
  Seq("BSD3" -> url("https://raw.githubusercontent.com/IndiscriminateCoding/api4s/dev/LICENSE"))
ThisBuild / homepage := Some(url("https://github.com/IndiscriminateCoding/api4s"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/IndiscriminateCoding/api4s"),
    "scm:git@github.com:IndiscriminateCoding/api4s.git"
  )
)
ThisBuild / developers := List(Developer(
  id = "IndiscriminateCoding",
  name = "IndiscriminateCoding",
  email = "28496046+IndiscriminateCoding@users.noreply.github.com",
  url = url("https://github.com/IndiscriminateCoding/")
))
