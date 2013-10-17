package edu.knowitall.prep

import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.tokenize.ClearTokenizer
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.postag.ClearPostagger
import scopt.OptionParser
import scala.io.Source
import java.io.PrintStream
import java.io.File
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import java.util.concurrent.atomic.AtomicInteger
import scala.Option.option2Iterable
import scala.collection.JavaConverters._
import util.FileUtils
import util.LineReader
import util.Line

class KbpSentenceParser() {

  val chunkerModel = OpenNlpChunker.loadDefaultModel
  val postagModel = OpenNlpPostagger.loadDefaultModel

  val chunkerLocal = new ThreadLocal[OpenNlpChunker] {
    override def initialValue = {
      val postagger = new OpenNlpPostagger(postagModel, tokenizer)
      new OpenNlpChunker(chunkerModel, postagger)
    }
  }

  lazy val tokenizer = new ClearTokenizer()

  lazy val parser = new ClearParser(new ClearPostagger(tokenizer))

  def parseKbpSentence(kbpSentence: KbpSentence): Option[ParsedKbpSentence] = {

    try {
    // chunks, then parse
    val chunker = chunkerLocal.get
    val chunked = chunker.chunk(kbpSentence.text)
    // Synchronize because the OpenNlpTokenizer isn't threadsafe
    val dgraph = parser.dependencyGraph(kbpSentence.text)
    val postags = chunked.map(_.postag).mkString(" ")
    val chunks = chunked.map(_.chunk)

    Some(
        new ParsedKbpSentence(
            kbpSentence.docId,
            kbpSentence.sentNum,
            kbpSentence.offset,
            chunks,
            dgraph))
    } catch {
      case e: Throwable =>
        System.err.println("Error parsing sentence: %s".format(kbpSentence.text))
        e.printStackTrace()
        None
    }
  }
}


object KbpSentenceParser {

  import java.util.concurrent.atomic.AtomicInteger

  private val sentencesProcessed = new AtomicInteger(0)

  def processXml(lines: Iterator[Line], corpus: String): Iterator[ParsedKbpSentence] = {
    val parser = new KbpSentenceParser
    val groupedSentences = Sentencer.processXml(lines, corpus).grouped(100)
    groupedSentences.flatMap { group =>
      group.par flatMap parser.parseKbpSentence
    }
  }

  object Settings {
    var inputFile = ""
    var corpus = ""
    var limit = Int.MaxValue
    var outputFile = "stdout"
    var recursive = false
  }

  def main(args: Array[String]): Unit = {

    val cliParser = new OptionParser() {
      arg("inputFile", "Raw XML input file or directory (recursive)", { str => Settings.inputFile = str })
      arg("corpus", "news, forum, or web", { str => Settings.corpus = str })
      opt("outputFile", "File: ParsedKbpSentences, default stdout", { str => Settings.outputFile = str })
      opt("limit", "Limit number of sentences output?", { str => Settings.limit = str.toInt })
    }

    if (!cliParser.parse(args)) return
    System.err.println("Parsed args: " + args.toList)

    val output = {
      if (Settings.outputFile.equals("stdout"))
        System.out
      else new PrintStream(Settings.outputFile, "UTF8")
    }

    val inputSources = FileUtils.getFilesRecursive(new File(Settings.inputFile)) map { f => LineReader.fromFile(f, "UTF8") }

    val nsTime = Timing.time {
      val parser = new KbpSentenceParser

      val sentencesGrouped = Sentencer.processXml(FileUtils.getLines(inputSources), Settings.corpus).take(Settings.limit).grouped(100)

      sentencesGrouped foreach { sentenceGroup =>
        sentenceGroup.par foreach { sentence =>
          if (sentencesProcessed.incrementAndGet() % 10000 == 0) System.err.println("%d sentences processed".format(sentencesProcessed.get))
          parser.parseKbpSentence(sentence) map ParsedKbpSentence.write foreach output.println
        }
      }
    }

    val seconds = Timing.Seconds.format(nsTime)
    System.err.println("Processed %d sentences in %s.".format(sentencesProcessed.get, seconds))

    output.close()
  }
}

