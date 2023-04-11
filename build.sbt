ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

Compile / run / mainClass := Some("rover.main")

lazy val root = (project in file("."))
  .settings(
    name := "rover",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
    )
  )
