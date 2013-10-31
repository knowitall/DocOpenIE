package edu.knowitall.tool.coref

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.link.Link
import edu.knowitall.tool.link.Linker
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

trait KBPRuleResolver extends CorefResolver with Linker {

  // the rule resolver takes as input a document that:
  type document = Document with
  // already has some coref info, such as from stanford
       CorefResolved with
  // and has been extracted via Open IE.
       Sentenced[Sentence with OpenIEExtracted]

  type mention = Mention

  // the link type may need to stay general if we're going to return freebase, nell, gazetteer links, etc...
  type link = Link
}