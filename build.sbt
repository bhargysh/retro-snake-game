name := "snake-game"

version := "0.1"

scalaVersion := "2.12.9"

//enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)
// This is an application with a main method
scalaJSUseMainModuleInitializer := true


jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv
npmDependencies in Compile += "jsdom" -> "13.1.0"
//jsDependencies += "org.webjars.npm" % "jsdom" % "13.1.0" / "jsdom.js" % Test

//scalaJSStage in Global := FullOptStage

val specs2Version = "4.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.1",
  "org.typelevel" %% "cats-effect" % "1.4.0",
  "org.specs2"    %% "specs2-core" % specs2Version % Test,
  "org.specs2"    %% "specs2-scalacheck" % specs2Version % Test,
  "org.scala-js" %%% "scalajs-dom" % "0.9.7",
  "org.scalatest" %%% "scalatest" % "3.2.0-M2" % Test,
  "org.scalatest" %%% "scalatest-funspec" % "3.2.0-M2" % Test,
//  "io.scalajs.npm" %%% "jsdom" % "0.5.0" % Test,
)
requireJsDomEnv in Test := true
testFrameworks := Seq(TestFrameworks.Specs2, TestFrameworks.ScalaTest)

//TODO: webpack config in sbt can override --> investigate