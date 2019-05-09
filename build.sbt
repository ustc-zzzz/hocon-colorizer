enablePlugins(ScalaJSPlugin)

name := "HoconColorizer"
scalaVersion := "2.11.12"

scalaJSUseMainModuleInitializer := true
scalaJSModuleKind := ModuleKind.CommonJSModule
mainClass in Compile := Some("com.github.ustc_zzzz.hocon.HoconColorizer")

libraryDependencies += "io.scalajs" %%% "nodejs" % "0.4.2"
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.1.0"