package edu.knowitall.tool.coref

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.tool.sentence.OpenIEExtracted

trait CorefResolver {

  type mention <: Mention

  type document <: Document

  def resolve(doc: document): Seq[MentionCluster[mention]]
}

class StanfordCorefResolver() extends CorefResolver {
  type mention = Mention
  type document = Document

  val coref = new StanfordCoreferenceResolver()

  override def resolve(doc: Document): Seq[MentionCluster[Mention]] = {
    val clusters = coref.clusters(doc.text)
    clusters.iterator.toSeq.map { case (bestMention, allMentions) =>
      new MentionCluster[Mention] {
        def best = bestMention
        def mentions = allMentions
      }
    }
  }
}