ThisBuild / scalaVersion := "2.13.5"
ThisBuild / organization := "com.superfell"

lazy val wsdl = (project in file("."))
  .settings(
    name := "WSDL",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
  )