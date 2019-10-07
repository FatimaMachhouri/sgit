name := "sgit"

version := "0.1"

scalaVersion := "2.13.1"

lazy val root = (project in file("."))
  .settings(
    name := "sgit",
    libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test",

  )

import sbtassembly.AssemblyPlugin.defaultUniversalScript
assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = false)))
assemblyJarName in assembly := s"${name.value}"