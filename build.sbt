Global / useSuperShell := false

lazy val scalaVersions = List("2.12.15", "2.13.7")

ThisBuild / version := Versions.api4s
ThisBuild / organization := "com.github.IndiscriminateCoding"
ThisBuild / scalaVersion := scalaVersions.head

ThisBuild / libraryDependencies +=
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

lazy val api4s = (project in file("."))
  .aggregate(codegen, core, `sbt-plugin`)
  .settings(
    publish / skip := true,
    crossScalaVersions := Nil
  )

lazy val codegen = (project in file("codegen"))
  .settings(
    Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.codegen).taskValue,
    crossScalaVersions := scalaVersions,
    name := "api4s-codegen",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-core" % Versions.http4s,
      "io.circe" %% "circe-yaml" % "0.14.1",
      "io.circe" %% "circe-generic" % Versions.circe,

      "org.scalatest" %% "scalatest" % "3.2.10" % "test"
    )
  )

lazy val core = (project in file("core"))
  .settings(
    Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.core).taskValue,
    crossScalaVersions := scalaVersions,
    name := "api4s-core",
    scalacOptions += "-language:higherKinds",
    scalacOptions ++= {
      if (scalaVersion.value.startsWith("2.12"))
        Seq("-Ypartial-unification")
      else Nil
    },
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-core" % Versions.http4s,
      "org.http4s" %% "http4s-circe" % Versions.http4s,
      "io.circe" %% "circe-generic" % Versions.circe
    )
  )

lazy val `sbt-plugin` = (project in file("sbt-plugin"))
  .settings(
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
