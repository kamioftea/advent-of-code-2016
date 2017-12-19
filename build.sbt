name := "advent-of-code-2016"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.smartystreets.api" % "smartystreets-java-sdk" % "3.2.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.8",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.8" % Test,
  "com.typesafe.akka" %% "akka-typed" % "2.5.8",// For Akka 2.4.x or 2.5.x
  "com.typesafe.akka" %% "akka-http" % "10.0.11",
  // Only when running against Akka 2.5 explicitly depend on akka-streams in same version as akka-actor
  "com.typesafe.akka" %% "akka-stream" % "2.5.8"// or whatever the latest version is
)
