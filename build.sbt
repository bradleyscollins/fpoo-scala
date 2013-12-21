name := "Chapter 1 Exercises"

organization := "fpoo"

version := "1.0"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.scalautils" % "scalautils_2.10" % "2.0"
)
