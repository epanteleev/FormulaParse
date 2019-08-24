name := "untitled"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "junit" % "junit" % "4.10" % Test
scalacOptions ++= Seq("-deprecation")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test"