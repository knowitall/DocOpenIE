import AssemblyKeys._

assemblySettings

name := "doc-openie"

organization := "edu.washington.cs.knowitall.openie"

version := "0.1-SNAPSHOT"

crossScalaVersions := Seq("2.10.2")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

libraryDependencies ++= Seq(
  "edu.washington.cs.knowitall.srlie" %% "srlie" % "1.0.3-SNAPSHOT",
  "edu.washington.cs.knowitall.chunkedextractor" %% "chunkedextractor" % "1.0.6",
  "edu.washington.cs.knowitall.openie" %% "openie-linker" % "1.1-SNAPSHOT",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-sentence-breeze" % "2.4.4",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-chunk-opennlp" % "2.4.4",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-parse-clear" % "2.4.4",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-coref-stanford" % "2.4.4",
  "edu.washington.cs.knowitall.taggers" %% "taggers-core" % "0.3",
  "com.twitter" % "chill_2.10" % "0.3.5")

scalacOptions ++= Seq("-unchecked", "-deprecation")

// custom options for high memory usage

javaOptions += "-Xmx4G"

javaOptions ++= Seq("-XX:+UseConcMarkSweepGC", "-Xmx8G", "-Xss512m")

fork in run := true

connectInput in run := true // forward stdin/out to fork
