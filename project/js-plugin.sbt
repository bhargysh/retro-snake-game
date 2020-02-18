addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.0")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.16.0")
dependencyOverrides ++= Seq("org.scala-js" %% "scalajs-linker" % "1.0.0")
//libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0-RC1"