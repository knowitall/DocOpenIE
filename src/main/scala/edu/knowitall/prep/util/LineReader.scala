package edu.knowitall.prep.util

import java.io.InputStream
import java.io.File
import io.Source

case class Line(val text: String, val terminator: String)

/**
 * A wrapper for io.Source that includes newlines.
 */
class LineReader private (source: Source) extends Iterator[Line] {

  private lazy val iter = source.buffered

  private val textBuffer = new StringBuilder
  private val termBuffer = new StringBuilder

  def getc() = iter.hasNext && {
    val ch = iter.next()
    if (ch == '\n') {
      termBuffer append ch
      false
    } else if (ch == '\r') {
      termBuffer append ch
      if (iter.hasNext && iter.head == '\n')
        termBuffer append iter.next()

      false
    } else {
      textBuffer append ch
      true
    }
  }
  def hasNext = iter.hasNext
  def next = {
    textBuffer.clear()
    termBuffer.clear()
    while (getc()) {}
    Line(textBuffer.toString, termBuffer.toString)
  }

  def close() = source.close()
}

object LineReader {

  def fromFile(filename: String, charset: String): LineReader = fromFile(new File(filename), charset)

  def fromFile(file: File, charset: String): LineReader = {
    new LineReader(io.Source.fromFile(file, charset))
  }

  def fromInputStream(istream: InputStream, charset: String): LineReader = {
    new LineReader(io.Source.fromInputStream(istream, charset))
  }

  def fromURL(url: java.net.URL, charset: String): LineReader = {
    new LineReader(io.Source.fromURL(url, charset))
  }

  def stdin = new LineReader(io.Source.stdin)
}