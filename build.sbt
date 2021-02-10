name := "scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"                % "3.1.2" % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
  "org.typelevel"     %% "cats-core"                % "2.2.0"
)
