name := "scalua"

organization := "scalua"

scalaVersion := "2.12.3"

fork := true

outputStrategy := Some(StdoutOutput)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Yinfer-argument-types", "-Xlint", "-Ypartial-unification", "-opt:_", "-opt-warnings:_")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0"
)

// vim: set ts=2 sw=2 et:
