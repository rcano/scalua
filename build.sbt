name := "scalua"

organization := "scalua"

scalaVersion := "2.11.7"

fork := true

outputStrategy := Some(StdoutOutput)

scalacOptions ++= Seq("-deprecation", "-feature", "-Yinfer-argument-types", "-Ybackend:GenBCode", "-Ydelambdafy:method", "-target:jvm-1.8")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-java8-compat" % "0.5.0"
)

// vim: set ts=2 sw=2 et:
