name := "snake-game"

version := "0.1"

scalaVersion := "2.12.9"

enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

useYarn := true

//jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv

val specs2Version = "4.9.2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.1",
  "org.typelevel" %% "cats-effect" % "1.4.0",
  "org.specs2"   %%% "specs2-core" % specs2Version % Test,
  "org.specs2"   %%% "specs2-scalacheck" % specs2Version % Test,
  "com.lihaoyi"  %%% "utest" % "0.7.4" % Test,
  "org.scala-js" %%% "scalajs-dom" % "1.0.0",
)

requireJsDomEnv in Test := true

testFrameworks := Seq(TestFrameworks.Specs2)