ThisBuild / scalaVersion := "3.4.2"

lazy val root = (project in file("."))
  .settings(
    name := "Uno2",
    organization := "de.htwg.se",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    )
  )
