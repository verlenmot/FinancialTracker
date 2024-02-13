ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.18"

lazy val root = (project in file("."))
  .settings(
    name := "FinancialTracker"
  )

libraryDependencies += "com.github.nscala-time" % "nscala-time_2.12" % "2.32.0"