package edu.knowitall.prep

import java.util.concurrent.atomic.AtomicInteger
import java.util.regex.Pattern
import edu.knowitall.tool.segment.Segmenter
import Sentencer._
import util.Line
import util.LineReader

/**
 * Converts from KbpParsedDoc to KbpSentences
 */
class Sentencer private (val segmenter: Segmenter) {

  private val errorCounter = new AtomicInteger(0)

  /**
   * Returns an empty collection on error.
   */
  def convertToSentences(parsedDoc: KbpProcessedDoc): Seq[KbpSentence] = {

    val docId = parsedDoc.extractDocId
    val author = parsedDoc.extractAuthor
    val date = parsedDoc.extractDate

    if (docId.isEmpty) {
      val msgFmt = "Sentencer error #%d: Doc skipped; Unable to extract docId from line: %s"
      val msg = msgFmt.format(errorCounter.incrementAndGet(), parsedDoc.docIdLine.line)
      System.err.println(msg)
      Seq.empty
    } else /* if docId.isDefined */ {
      buildKbpSentences(docId.get, author, date, parsedDoc.textLines)
    }
  }

  private val newLine = Pattern.compile("\n|\t")

  private def buildParagraphs(lineIterator: Iterator[KbpDocLine]): Seq[KbpDocLine] = {
    val lineGroups = Iterator.continually {
      lineIterator.dropWhile(_.isBlank).takeWhile(!_.isBlank).toSeq
    } takeWhile(!_.isEmpty) toSeq

    // join lines of a paragraph into big strings, and stuff into a KbpDocLine
    val paragraphs = lineGroups map { lineGroup =>
      // join into one big KbpDocLine
      // assume lines are in document order
      val text = lineGroup.map(_.line).mkString
      //val text = new String(bytes, "UTF8")
      val offset = lineGroup.head.offset
      new KbpDocLine(text, offset)
    }
    paragraphs
  }

  private def buildKbpSentences(docId: String, author: Option[KbpDocLine], date: Option[KbpDocLine], textLines: Seq[KbpDocLine]): Seq[KbpSentence] = {

    // first, return any author or date lines as sentences
    val optionals = (author.toSeq ++ date.toSeq)

    // split textLines into groups (rough paragraphs?) around empty lines (double-newlines)
    val paragraphs = buildParagraphs(textLines.iterator)

    // run segmenter on each paragraph,
    // map resulting segments to the KbpDocLine structure.
    // flatten over all paragraphs
    val allSegments = paragraphs map { pgraph =>
      val segments = try {
        segmenter.segment(pgraph.line).toSeq
      } catch {
        case e: Throwable => {
          System.err.println("Sentencer error #%d: Segmenter exception on input: %s".format(errorCounter.incrementAndGet(), pgraph.line))
          e.printStackTrace()
          Seq.empty
        }
      }
      (segments, pgraph.offset)
    }

    val asDocLines = allSegments.flatMap { case (segments, pgraphStart) =>
      val segStarts = (0 to segments.size - 1).map { num =>
        val segOffset = segments.take(num).map(_.length).sum
        segOffset + pgraphStart
      }
      assert(segments.size == segStarts.size)
      segments.zip(segStarts) map { case (seg, start) =>
        new KbpDocLine(seg.text, start)
      }
    }

    // convert KbpDocLines to KbpSentences.
    (optionals ++ asDocLines).zipWithIndex map { case (kbpLine, sentNum) =>
      new KbpSentence(docId, sentNum, kbpLine.offset, newLine.matcher(kbpLine.line).replaceAll(" "))
    }
  }
}

object Sentencer {

  import scopt.OptionParser

  import edu.knowitall.tool.sentence.BreezeSentencer

  lazy val defaultInstance = new Sentencer(new BreezeSentencer())

  def processXml(lines: Iterator[Line], corpus: String): Iterator[KbpSentence] = {
    KbpDocProcessor.processXml(lines, corpus) flatMap defaultInstance.convertToSentences flatMap SentenceFilter.apply
  }

  def main(args: Array[String]): Unit = {

    var inputFile = "."
    var outputFile = "stdout"
    var corpus = "."
    var filter = false

    val parser = new OptionParser("TAC-2013 Sentencer") {

      arg("inputFile", "Input File.", { str => inputFile = str})
      arg("corpus", "news, forum, or web.", { str => corpus = str})
      opt("outputFile", "File for output, default stdout", { str => outputFile = str})
      opt("filter", "Run sentences through SentenceFilter, default=false.", { filter = true })
    }

    if (!parser.parse(args)) return

    var news = corpus.equals("news")
    var forum = corpus.equals("forum")
    var web = corpus.equals("web")
    if (!news && !forum && !web) throw new IllegalArgumentException("Unknown corpus: %s".format(args(1)))

    val docParser = KbpDocProcessor.getProcessor(corpus)
    val sentencer = defaultInstance

    val lineReader = LineReader.fromFile(inputFile, "UTF8")
    val output = if (outputFile.equals("stdout")) System.out else new java.io.PrintStream(outputFile)

    val docs = new DocSplitter(lineReader)
    val parsedDocs = docs flatMap docParser.process
    val allSentences = parsedDocs flatMap sentencer.convertToSentences
    val filteredSentences = if (filter) allSentences flatMap SentenceFilter.apply else allSentences

    filteredSentences foreach { s =>
        output.println(KbpSentence.write(s))
    }

    lineReader.close()
    output.close()
  }
}
