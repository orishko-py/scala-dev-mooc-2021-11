import sbt.Keys.libraryDependencies

import Dependencies._


lazy val root = (project in file("."))
  .settings(
    name := "scala-dev-mooc-2021-11",
    version := "0.1",
    scalaVersion := "2.13.3",
    libraryDependencies ++= Seq(
      kindProjector
    ),
    addCompilerPlugin(Dependencies.kindProjector)
  )

// testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

scalacOptions += "-Ymacro-annotations"

libraryDependencies += "org.scalatest"  %% "scalatest"    % "3.2.10" % "test"
libraryDependencies += "org.mockito"    % "mockito-core"  % "3.0.0" % "test"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.3"