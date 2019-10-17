name := "sgit"

version := "0.1"

scalaVersion := "2.13.1"

lazy val root = (project in file("."))
  .settings(
    name := "sgit",
    libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"
  )

import sbtassembly.AssemblyPlugin.defaultUniversalScript
assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = false)))
assemblyJarName in assembly := s"${name.value}"

import sbtsonar.SonarPlugin.autoImport.sonarProperties
sonarProperties ++= Map(
  "sonar.host.url" -> "http://localhost:9000",
  "sonar.sources" -> "src/main/scala",
  "sonar.tests" -> "src/test/scala"
)

parallelExecution in Test := false