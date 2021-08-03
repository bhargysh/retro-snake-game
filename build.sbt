name := "snake-game"

version := "0.1"

scalaVersion := "2.12.9"

enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

useYarn := true

//jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv

val specs2Version = "4.9.2"
val catsVersion = "2.1.1"
val catsEffectVersion = "2.1.4"

libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats-core" % catsVersion,
  "org.typelevel" %%% "cats-effect" % catsEffectVersion,
  "org.typelevel" %%% "cats-laws" % catsVersion % Test,
  "org.typelevel" %%% "cats-effect-laws" % catsEffectVersion % Test,
  "org.typelevel" %%% "discipline-specs2" % "1.1.6" % Test,
  "org.specs2"   %%% "specs2-core" % specs2Version % Test,
  "org.specs2"   %%% "specs2-cats" % specs2Version % Test,
  "org.specs2"   %%% "specs2-scalacheck" % specs2Version % Test,
  "com.lihaoyi"  %%% "utest" % "0.7.4" % Test,
  "org.scala-js" %%% "scalajs-dom" % "1.0.0",
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)

requireJsDomEnv in Test := true

testFrameworks := Seq(TestFrameworks.Specs2)

scalacOptions --= Seq("-Ywarn-value-discard")