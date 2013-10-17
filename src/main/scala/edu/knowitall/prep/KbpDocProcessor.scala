package edu.knowitall.prep

import java.util.regex.Pattern
import java.util.LinkedList
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._
import util._
import java.io.File

/**
 * Converts KbpRawDocs into KbpProcessedDocs,
 * which contain only the KbpDocLines that are relevant.
 */
abstract class KbpDocProcessor() {

  protected val errorCounter = new AtomicInteger(0)

  def process(rawDoc: KbpRawDoc): Option[KbpProcessedDoc]

  protected def buildDoc(
    docIdLine: Option[KbpDocLine],
    authorLine: Option[KbpDocLine],
    dateLine: Option[KbpDocLine],
    textLines: List[KbpDocLine]): Option[KbpProcessedDoc] = {

    if (docIdLine.isEmpty) {
      val className = KbpDocProcessor.this.getClass().getName()
      val msg = "%s error #%d, no docId found. Sample text follows...".format(className, errorCounter.incrementAndGet())
      System.err.println(msg)
      for (kbpline <- textLines.take(5)) System.err.print(kbpline.line)
      None
    } else /* if docIdLine.isDefined */ {
      Some(new KbpProcessedDoc(docIdLine.get, authorLine, dateLine, textLines))
    }
  }

  protected def isValidText(line: String): Boolean = {
    !line.startsWith("<")
  }
}

object KbpDocProcessor {

  def getProcessor(corpus: String) = corpus match {
    case "web" => KbpWebDocProcessor
    case "news" => KbpNewsDocProcessor
    case "forum" => KbpForumDocProcessor
    case _ => throw new IllegalArgumentException("Unknown corpus type \"%s\"".format(corpus))
  }

  def processXml(lines: Iterator[Line], corpus: String): Iterator[KbpProcessedDoc] = {

    val docSplitter = new DocSplitter(lines)

    val processor = getProcessor(corpus)

    docSplitter flatMap processor.process
  }

  def main(args: Array[String]): Unit = {

    val inputFile = args(0)
    val outputFile = args(1)
    val corpus = args(2) // web, forum, or news

    val output = new java.io.PrintStream(outputFile, "UTF8")

    val input = LineReader.fromFile(inputFile, "UTF8")

    processXml(input, corpus) foreach { parsedDoc =>
      val text = parsedDoc.debugText
      output.println(text)
    }

    output.close()

  }
}

object KbpForumDocProcessor extends KbpDocProcessor {
  override def process(rawDoc: KbpRawDoc): Option[KbpProcessedDoc] = {

    var docIdLine = Option.empty[KbpDocLine]
    var textLines = new LinkedList[KbpDocLine]

    var lastLineEmpty = false

    for (kbpLine <- rawDoc.lines; line = kbpLine.line) {

      if (docIdLine.isEmpty && line.startsWith("<doc id")) docIdLine = Some(kbpLine)
      else if (isValidText(line)) textLines.add(kbpLine)
      else textLines.add(kbpLine.copy(line = "\n"))
    }

    buildDoc(docIdLine, None, None, textLines.asScala.toList)
  }
}

object KbpNewsDocProcessor extends KbpDocProcessor {

  override def process(rawDoc: KbpRawDoc): Option[KbpProcessedDoc] = {

    var docIdLine = Option.empty[KbpDocLine]
    var dateLine = Option.empty[KbpDocLine]
    var textLines = new LinkedList[KbpDocLine]

    var datelineNext = false

    for (kbpLine <- rawDoc.lines; line = kbpLine.line) {

      if (datelineNext) {
        dateLine = Some(kbpLine)
        datelineNext = false
      } else if (docIdLine.isEmpty && line.startsWith("<DOC id")) docIdLine = Some(kbpLine)
      else if (dateLine.isEmpty && line.startsWith("<DATELINE>")) datelineNext = true
      else if (isValidText(line)) textLines.add(kbpLine)
      else textLines.add(kbpLine.copy(line = "\n"))
    }

    buildDoc(docIdLine, None, dateLine, textLines.asScala.toList)
  }
}

object KbpWebDocProcessor extends KbpDocProcessor {

  override def process(rawDoc: KbpRawDoc): Option[KbpProcessedDoc] = {

    var docIdLine = Option.empty[KbpDocLine]
    var authorLine = Option.empty[KbpDocLine]
    var dateLine = Option.empty[KbpDocLine]
    var textLines = new LinkedList[KbpDocLine]

    for (kbpLine <- rawDoc.lines; line = kbpLine.line) {

      if (docIdLine.isEmpty && line.startsWith("<DOCID>")) docIdLine = Some(kbpLine)
      else if (authorLine.isEmpty && line.startsWith("<POSTER>")) authorLine = Some(kbpLine)
      else if (dateLine.isEmpty && line.startsWith("<DATETIME>")) dateLine = Some(kbpLine)
      else if (isValidText(line)) textLines.add(kbpLine)
      else textLines.add(kbpLine.copy(line = "\n"))
    }

    buildDoc(docIdLine, authorLine, dateLine, textLines.asScala.toList)
  }
}

