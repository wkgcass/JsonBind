organization := "net.cassite"

name := "JsonBind"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "net.cassite" % "style" % "2.0.1"

libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.4.6"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.12"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.12" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0-M14" % "test"