ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
      name := "Uno2",
      organization := "de.htwg.se",
      version := "0.1.0",

      libraryDependencies ++= Seq(
          "org.scalatest" %% "scalatest" % "3.2.19" % Test
      )
  )
