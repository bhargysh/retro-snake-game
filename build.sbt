name := "snake-game"

enablePlugins(ScalaJSBundlerPlugin) // , ScalaJSJUnitPlugin

scalaVersion := "2.12.9"
val specs2Version = "4.7.0"

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.1",
  "org.typelevel" %% "cats-effect" % "1.4.0",
  "org.specs2"    %% "specs2-core" % specs2Version % Test,
  "org.specs2"    %% "specs2-scalacheck" % specs2Version % Test,
)

npmDependencies in Compile += "snabbdom" -> "0.5.3"

npmDevDependencies in Compile += "uglifyjs-webpack-plugin" -> "1.2.2"

// Use a different Webpack configuration file for production
webpackConfigFile in fullOptJS := Some(baseDirectory.value / "prod.webpack.config.js")

// Execute the tests in browser-like environment
requireJsDomEnv in Test := true

webpackBundlingMode := BundlingMode.LibraryAndApplication()

useYarn := true

// HtmlUnit does not support ECMAScript 2015
scalaJSLinkerConfig ~= { _.withESFeatures(_.withUseECMAScript2015(false)) }

ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet

//TODO: try one last test framework JUnit or uTest