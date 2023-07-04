ThisBuild / scalaVersion := "2.13.10"
ThisBuild / organization := "cazzola@adapt-lab"

lazy val es3 = (project in file("."))
  .settings(
    name := "es3",
    scalacOptions += "-deprecation",
    libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % "2.4.1",
  )