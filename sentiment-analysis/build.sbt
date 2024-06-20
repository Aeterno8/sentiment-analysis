import Dependencies._

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "sentiment-analysis",
    libraryDependencies ++= Seq(
      "org.jfree" % "jfreechart" % "1.5.3",
    ),
    libraryDependencies += munit % Test
  )

javaOptions += "-Xmx4G"
javaOptions += "-Djava.awt.headless=true"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
