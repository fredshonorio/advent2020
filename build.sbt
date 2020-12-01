import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.eggsample"
ThisBuild / organizationName := "eggsample"

lazy val root = (project in file("."))
  .settings(
    name := "advent2020",
    libraryDependencies += scalaTest % Test
  )
