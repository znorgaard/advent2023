ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent2023",
    artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) => "advent.jar" }
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"