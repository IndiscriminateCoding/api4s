ThisBuild / version := "0.0.0"
ThisBuild / organization := "com.github.IndiscriminateCoding"
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-language:higherKinds",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:infer-any",
  "-Xlint:nullary-unit",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

lazy val tests = (project in file("."))
  .enablePlugins(Api4s)
  .settings(
    libraryDependencies ++= Seq(
      organization.value %% "api4s-core" % Versions.api4s,
      "org.http4s" %% "http4s-client" % Versions.http4s
    ),
    api4sSources :=
      CodegenTests.download((Compile / sourceManaged).value) flatMap { case (n, f, s) =>
        val src = Api4s.Src(f, n, server = s)
        val noErr = src.copy(pkg = "noerr." ++ src.pkg).without4xx.without5xx
        val noDef = noErr.copy(pkg = "nodef." ++ src.pkg).withoutDefault
        List(src, noErr, noDef)
      }
  )
