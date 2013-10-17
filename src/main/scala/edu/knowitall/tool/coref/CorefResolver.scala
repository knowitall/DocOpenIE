package edu.knowitall.tool.coref

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.Mention
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

trait KBPRuleResolver extends CorefResolver with Linker {

  // the rule resolver takes as input a document that:
  type document = Document with
  // already has some coref info, such as from stanford
       CorefResolved[Mention] with
  // and has been extracted via Open IE.
       Sentenced[Sentence with OpenIEExtracted]

  type mention = Mention

  // the link type may need to stay general if we're going to return freebase, nell, gazetteer links, etc...
  type link = Link
}