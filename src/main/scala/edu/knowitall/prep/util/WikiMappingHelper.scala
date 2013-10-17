package edu.knowitall.prep.util

import scopt.OptionParser
import java.io.File
import java.io.PrintStream
import edu.knowitall.common.Timing
import java.util.concurrent.atomic.AtomicInteger

/**
 * A tool for extracting from the TAC initial knowledge base, for each entity:
 * Node Id, ( a TAC-specific identifier, e.g. E0000001
 * Wiki Name ( wikipedia page name )
 */
object WikiMappingHelper {

  val entityCounter = new AtomicInteger(0)

  def main(args: Array[String]): Unit = {

    var inputFile = "."
    var output = System.out

    val parser = new OptionParser() {
      arg("inputFile", "A file, or directory in which to look for files (recursively).", { s => inputFile = s })
      opt("outputFile", "Optional file for output, default stdout.", { s => output = new PrintStream(s) })
    }

    if (!parser.parse(args)) return
    else {
      val inputs = FileUtils.getFilesRecursive(new File(inputFile))
      val nsTime = Timing.time {
        run(inputs, output)
      }
      System.err.println(s"Processed ${entityCounter.get} entities in ${Timing.Seconds.format(nsTime)}.")
    }
  }

  def run(files: Iterator[File], output: PrintStream): Unit = {

    val inputSources = files map { f => LineReader.fromFile(f, "UTF8") }
    val inputLines = FileUtils.getLines(inputSources).map(_.text)

    val entityInfos = inputLines.grouped(10000).flatMap { case bigGroup =>
      val smallGroups = bigGroup.grouped(100).toSeq
      smallGroups.par flatMap { smallGroup =>
        smallGroup flatMap processLine
      }
    }
    entityInfos map (_.serialize) foreach { e =>
      entityCounter.incrementAndGet()
      output.println(e)
    }
  }

  val entityRegex = "<entity wiki_title=\"([^\"]+)\" type=\"([^\"]+)\" id=\"([^\"]+)\" name=\"([^\"]+)\">".r

  case class EntityInfo(val id: String, val name: String, val typ: String) {
    def serialize = Seq(id, name, typ).mkString("\t")
  }

  def processLine(line: String): Option[EntityInfo] = {
    line match {
      case entityRegex(title, typ, id, name) => Some(EntityInfo(id, name, typ))
      case _ => None
    }
  }

  def loadNameToNodeIdMap(lines: Iterator[String]): Map[String, String] = {
    System.err.println("Loading wikipedia name to node id map...")
    val tabSplit = "\t".r
    lines.map { line =>
      tabSplit.split(line) match {
        case Array(id, name, typ, _*) => (name, id)
        case _ => throw new RuntimeException(s"Error parsing entity info: $line")
      }
    } toMap

  }
}