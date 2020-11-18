name := "Firstorder"

version := "0.1"

Compile / scalaSource := baseDirectory.value / "src"
Test / scalaSource := baseDirectory.value / "test-src"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.2.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "com.github.scopt" %% "scopt" % "4.0.0-RC2")
