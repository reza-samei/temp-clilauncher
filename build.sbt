
organization := "xyz.sigmalab.xlib"

name := "clilauncher"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.0-M4"

crossScalaVersions := Seq("2.11.12", "2.12.6", "2.13.0-M4")

// https://github.com/scalatest/scalatest/issues/1367
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.6-SNAP1" % Test

// https://github.com/scalaz/scalaz/issues/1719
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M23"

scalacOptions ++= Seq("-deprecation", "-feature")

