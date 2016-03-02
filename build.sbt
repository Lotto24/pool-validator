import java.time.format.DateTimeFormatter
import java.time.{Clock, ZonedDateTime}

enablePlugins(JavaAppPackaging)

val versions = Map(
  "akka" -> "2.4.0",
  "akka-http" -> "1.0",
  "slf4j" -> "1.7.10",
  "bouncycastle" -> "1.51"
)

lazy val commonDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % versions("akka"),
  "com.typesafe.akka" %% "akka-slf4j" % versions("akka"),
  "org.slf4j" % "slf4j-api" % versions("slf4j"),
  "org.slf4j" % "jcl-over-slf4j" % versions("slf4j"),
  "org.slf4j" % "log4j-over-slf4j" % versions("slf4j"),
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "io.spray" %% "spray-json" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "commons-io" % "commons-io" % "2.4",
  "commons-codec" % "commons-codec" % "1.10",
  "commons-configuration" % "commons-configuration" %"1.10",
  "org.apache.commons" % "commons-compress" % "1.8",
  "org.scalafx" % "scalafxml-core-sfx8_2.11" % "0.2.2",
  "org.jfxtras" % "jfxtras-controls" % "8.0-r4",
  "org.bouncycastle" % "bcpkix-jdk15on" % versions("bouncycastle"),
  "org.bouncycastle" % "bcprov-jdk15on" % versions("bouncycastle"),
  "com.typesafe.play" % "play-json_2.11" % "2.4.6",
  // test dependencies follow
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "junit" % "junit" % "4.10" % "test"

)

unmanagedBase := baseDirectory.value / "lib"

lazy val versionDateFormat = DateTimeFormatter.ofPattern("yyyyMMddhhmmss")

lazy val commonSettings = Seq(
  version := s"0.1.${versionDateFormat.format(ZonedDateTime.now(Clock.systemUTC()))}",
  scalaVersion := "2.11.7",
  libraryDependencies ++= commonDependencies,
  testOptions in Test <+= (target in Test) map { t =>
    Tests.Argument(TestFrameworks.ScalaTest, "-oDS", "-u", (t / "test-reports").toString)
  }
)

lazy val pool_validator = (project in file("."))
  .settings(commonSettings: _*)
