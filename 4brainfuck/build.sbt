ThisBuild / scalaVersion := "2.13.10"
ThisBuild / organization := "cazzola@adapt-lab"

lazy val brainfuck = (project in file("."))
  .settings(
    name := "BrainfuckInterpreter",
    scalacOptions += "-deprecation",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  )