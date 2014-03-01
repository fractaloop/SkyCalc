name := "skycalc"

version := "1.0"

version := "0.0-SNAPSHOT"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.6",
  "ch.qos.logback" % "logback-classic" % "1.1.1",
  "com.github.scopt" %% "scopt" % "3.2.0",
  "com.github.nscala-time" %% "nscala-time" % "0.8.0",
  "org.scalatest" %% "scalatest" % "2.0" % "test"
)
