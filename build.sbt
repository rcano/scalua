name := "scalua"

inThisBuild(Seq(
  organization := "scalua",
  scalaVersion := "2.12.4",
  fork := true,
  outputStrategy := Some(StdoutOutput),
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Yinfer-argument-types", "-Yno-predef", "-Yno-adapted-args", "-Xlint", "-Ypartial-unification", "-opt:_", "-opt-warnings:_", "-Ywarn-dead-code", "-Ywarn-extra-implicit", "-Ywarn-inaccessible", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-numeric-widen", "-Ywarn-unused:_", "-Ywarn-unused:-locals,-params,-explicits"),
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:_", "-opt:_", "-Xlint")
))


libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0"
)

lazy val scalua = Project("scalua", file("."))
lazy val libraryExtension = project.dependsOn(scalua)

// vim: set ts=2 sw=2 et:
