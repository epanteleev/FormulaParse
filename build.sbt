name := "untitled"

version := "0.1"

scalaVersion := "2.11.12"

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % Test
scalacOptions ++= Seq("-deprecation")
// for funsets
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
