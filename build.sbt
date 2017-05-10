// This is a common build SBT project definition. It includes all the standard
// settings, courtesy of the `sbt-cake` plugin (viz `project/cake.sbt`)

import sbt.Keys._

version := "1.0.0-SNAPSHOT"

// @protocol@
//
// The protocols are defined in the directory that contains the
// *.proto files. The Protocol Buffers plugin (see `project/protoc.sbt`)
// generates the matching Scala code.
//

// Hook up the PB generator task to the Compile task
PB.targets in Compile := Seq(
  scalapb.gen(flatPackage = true) -> (sourceManaged in Compile).value
)

libraryDependencies ++= Seq(
  "com.trueaccord.scalapb" %% "scalapb-runtime" % com.trueaccord.scalapb.compiler.Version.scalapbVersion,

  // For tests, we are using ScalaCheck
  "org.scalacheck" %% "scalacheck" % "1.13.4",

  // STest
  "org.scalatest" %% "scalatest"  % "3.0.1"
)
