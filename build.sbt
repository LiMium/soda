import sbt.Keys._


name := "soda"

version := "0.1"

scalaVersion in ThisBuild := "2.13.2"

scalacOptions in ThisBuild := List("-deprecation", "-feature", "-language:implicitConversions", "-language:reflectiveCalls")

