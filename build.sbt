name := "snake-game"

version := "0.1"

scalaVersion := "2.12.9"

enablePlugins(ScalaJSPlugin)
// This is an application with a main method
scalaJSUseMainModuleInitializer := true

val specs2Version = "4.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.1",
  "org.typelevel" %% "cats-effect" % "1.4.0",
  "org.specs2"    %% "specs2-core" % specs2Version % Test,
  "org.scala-js" %%% "scalajs-dom" % "0.9.7"
)

testFrameworks := Seq(TestFrameworks.Specs2)