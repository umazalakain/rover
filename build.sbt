ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

mainClass in (Compile, run) := Some("Rover")

lazy val root = (project in file("."))
  .settings(
    name := "rover"
  )
