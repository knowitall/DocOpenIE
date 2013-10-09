package edu.knowitall.docopenie

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.typer.Type
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.parse.graph.DependencyGraph

trait Link {
  def name: String
  def score: Double
}

trait TipsterGazetteerLink extends Link

trait FreeBaseLink extends Link {
  def id: String
  def types: Set[String]
}

trait Document {
  def sentences: IndexedSeq[Sentence]
}

trait Sentence {
  def text: String
  def chunkedTokens: Seq[ChunkedToken]
  def dependencyGraph: DependencyGraph
  def index: Int
}

trait SentenceTag extends Type {
  def sentence: Sentence
}

trait ScoredSentenceTag extends Type {
  def score: Double
}

trait CorefCluster {
  def mentions: Set[ScoredSentenceTag]
  def links: Set[Link]
}

trait Argument extends SentenceTag {
  override val name = "Argument"
  def corefCluster: CorefCluster
}

trait Relation extends SentenceTag {
  override val name = "Relation"
}

trait Extraction {
  def arg1: Argument
  def rel:  Relation
  def arg2s: Set[Argument]
  def sentence: Sentence
  def score: Double
}

trait ExtractedDocument extends Document {
  def types: Set[SentenceTag]
  def corefClusters: Set[CorefCluster]
  def extractions: Set[Extraction]
}
