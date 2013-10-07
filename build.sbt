import AssemblyKeys._

assemblySettings

name := "doc-openie"

organization := "edu.washington.cs.knowitall.openie"

version := "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.10.2")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

libraryDependencies ++= Seq(
  "edu.washington.cs.knowitall.openie" %% "openie" % "4.1",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-chunk-opennlp" % "2.4.3",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-parse-clear" % "2.4.3",
  "edu.washington.cs.knowitall.taggers" %% "taggers-core" % "0.3")

scalacOptions ++= Seq("-unchecked", "-deprecation")

// custom options for high memory usage

javaOptions += "-Xmx4G"

javaOptions += "-XX:+UseConcMarkSweepGC"

fork in run := true

connectInput in run := true // forward stdin/out to fork

licenses := Seq("Ollie Software License Agreement" -> url("https://raw.github.com/knowitall/ollie/master/LICENSE"))
