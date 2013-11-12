package edu.knowitall.tool.coref

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.tool.sentence.OpenIEExtracted

trait CorefResolver {

  def resolve(doc: Document): Seq[MentionCluster]
}

case class StanfordMentionCluster(val best: Mention, val mentions: Seq[Mention]) extends MentionCluster

class StanfordCorefResolver() extends CorefResolver {

  val coref = new StanfordCoreferenceResolver()

  override def resolve(doc: Document): Seq[MentionCluster] = {
    val clusters = coref.clusters(doc.text)
    clusters.iterator.toSeq.map { case (bestMention, allMentions) =>
      StanfordMentionCluster(bestMention, allMentions)
    }
  }
}