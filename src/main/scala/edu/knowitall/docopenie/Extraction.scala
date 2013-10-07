package edu.knowitall.docopenie

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.typer.Type

trait Link {
  def name: String
  // TODO: def mention: Mention
  def score: Double
}

case class FreeBaseEntity(name: String, id: String, types: Seq[String])

case class FreeBaseLink(entity: FreeBaseEntity, score: Double) extends Link {
  def name = entity.name
}

trait Sentence {
  def originalText: String
  def chunkedTokens: Seq[Lemmatized[ChunkedToken]]
  def dependencyGraph: DependencyGraph

  // parent pointers
  def sourceDocument: Document
  def sentenceIndex: Int
}


trait TaggedSentence extends Sentence {
  def tags: Seq[Type]
}

trait DocumentSpan {
  def sentence: Sentence
  def tokenInterval: Interval
}

trait Mention {
  def text: String
  def docSpan: DocumentSpan
  def confidence: Double

  // parent pointer
  def sourceChain: MentionChain
}

trait MentionChain {
  def mentions: Seq[Mention]
  def link: Seq[Link]
}

trait Document {
  def sentences: Seq[Sentence]
}

trait ExtractedDocument extends Document {
  def sentences: Seq[TaggedSentence]
  def mentionChains: Seq[MentionChain]
  def extractions: Seq[Extraction]
}

trait Extraction {
  def arg1: Argument
  def rel: Relation
  def arg2s: Seq[Argument]
  def confidence: Double

  // parent pointer
  def sentence: Sentence
}

trait Part {
  def text: String
  def tokenInterval: Interval

  // parent pointer
  def sourceExtraction: Extraction
}

trait Relation extends Part

trait Argument extends Part {
  def mention: Mention
}

