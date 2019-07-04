enablePlugins(ScalaJSPlugin)

organization := "me.shadaj"

name := "scalajs-loader"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-js" %%% "scalajs-linker" % "1.0.0-M6"

scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
