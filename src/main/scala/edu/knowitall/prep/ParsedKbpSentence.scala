package edu.knowitall.prep

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.collection.immutable.Interval
import KbpSentence.tabRegex
import edu.knowitall.tool.parse.graph.DependencyGraph


class ParsedKbpSentence(
    val docId: String,
    val sentNum: Int,
    val startOffset: Int,
    val chunks: Seq[String],
    val dgraph: DependencyGraph) {

  import ParsedKbpSentence.wsSplit

  require(chunks.size == dgraph.nodes.size)

  lazy val startOffsetInt = startOffset.toInt

  def chunkedTokens = {
    val postaggedTokens = dgraph.nodes.toSeq
    postaggedTokens.zip(chunks) map { case (p, chunk) =>
      new ChunkedToken(Symbol(chunk), p.postagSymbol, p.string, p.offset)
    }
  }

  def chunkedTokens(interval: Interval) = {
    val postaggedTokens = dgraph.nodes.toSeq.drop(interval.start)
    postaggedTokens.zip(chunks.drop(interval.start)).take(interval.length) map { case (p, chunk) =>
      new ChunkedToken(Symbol(chunk), p.postagSymbol, p.string, p.offset)
    }
  }
}

object ParsedKbpSentence {

  private val wsSplit = "\\s+".r

  val NUM_FIELDS = 5

  import KbpSentence.tabRegex

  def read(pickle: String): Option[ParsedKbpSentence] = read(tabRegex.split(pickle))

  def read(split: Seq[String]): Option[ParsedKbpSentence] = {
    split match {
      case Seq(docId, sentNum, startOffset, chunks, dgraph, _*) =>
        Some(new ParsedKbpSentence(docId, sentNum.toInt, startOffset.toInt, wsSplit.split(chunks), DependencyGraph.stringFormat.read(dgraph)))
      case _ => {
        System.err.println("Error reading ParsedKbpSentence: %s".format(split.mkString("\t")))
        None
      }
    }
  }

  def write(s: ParsedKbpSentence): String = {
    Seq(
      s.docId,
      s.sentNum.toString,
      s.startOffset.toString,
      s.chunks.mkString(" "),
      DependencyGraph.stringFormat.write(s.dgraph)
    ).map(_.replaceAll("\t", " ")).mkString("\t")
  }
}