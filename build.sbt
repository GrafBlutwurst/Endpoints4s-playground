import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "ch.grafblutwurst"
ThisBuild / organizationName := "grafblutwurst"

lazy val root = (project in file("."))
  .settings(
    name := "endpoints4sPlayground",
    libraryDependencies ++= Seq(
      "org.endpoints4s" %% "algebra" % "1.6.0",
      "org.endpoints4s" %% "http4s-server" % "8.0.0",
      "org.endpoints4s" %% "http4s-client" % "6.0.0",
    )
  )

