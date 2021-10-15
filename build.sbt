name := "scalua"

inThisBuild(Seq(
  organization := "scalua",
  scalaVersion := "3.1.0-RC3",
  fork := true,
  outputStrategy := Some(StdoutOutput),
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-Yno-predef"),
))


libraryDependencies ++= Seq(
)

lazy val scalua = Project("scalua", file("."))
lazy val libraryExtension = project.dependsOn(scalua)

// vim: set ts=2 sw=2 et:
