package edu.knowitall.prep

import java.io.File
import edu.knowitall.common.Resource
import java.util.LinkedList
import java.util.regex.Pattern
import scala.collection.JavaConverters._
import util.LineReader
import util.Line
import java.io.PrintStream

/**
 * Reads KBP corpus and provides an iterator over
 * document elements (<DOC>) in the corpus.
 */
class DocSplitter(lines: Iterator[Line]) extends Iterator[KbpRawDoc] {

  import java.io.PrintStream

  private val docCloseTag = Pattern.compile("\\s*(</DOC>|</doc>)\\s*")

  def hasNext = lines.hasNext

  def next: KbpRawDoc = {

    val lineBuffer = new LinkedList[KbpDocLine]()

    var offset = 0

    var done = false

    while (!done && lines.hasNext) {

      val nextLine = lines.next()
      val fullLineString = nextLine.text + nextLine.terminator
      if (docCloseTag.matcher(fullLineString).matches()) done = true
      lineBuffer.add(new KbpDocLine(fullLineString, offset))
      offset = offset + fullLineString.length
    }

    new KbpRawDoc(lineBuffer.asScala.toList)
  }
}

object DocSplitter {

  def main(args: Array[String]) {

    val inputFile = args(0)
    val outputDir = args(1)
    val docsToSplitStr = args(2)

    val docsToSplit = if (docsToSplitStr.equals("-1")) Int.MaxValue else docsToSplitStr.toInt

    var numDocs = 0

    val lineReader = LineReader.fromFile(inputFile, "UTF8")
    val docSpliterator = new DocSplitter(lineReader).take(docsToSplit)

    docSpliterator.foreach { kbpDoc =>
      val output = new PrintStream("%s/doc%d.txt".format(outputDir, numDocs), "UTF8")
      kbpDoc.lines.foreach { kbpLine => output.print(kbpLine.line) }
      numDocs += 1
    }

    lineReader.close()
  }
}