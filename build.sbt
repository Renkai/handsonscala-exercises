name := "handsonscala-exercises"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.1"

testFrameworks += new TestFramework("utest.runner.Framework")
