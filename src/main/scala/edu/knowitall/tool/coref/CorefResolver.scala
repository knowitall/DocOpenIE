package edu.knowitall.tool.coref

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.Mention
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.coref.Linked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Extracted

trait CorefResolver {

  type mention <: Mention

  type document <: Document

  def resolve(doc: Document): Seq[MentionCluster[mention]]
}

trait KBPRuleResolver extends CorefResolver {

  type LinkedMention = Mention with Linked

  type LinkedMentionCluster = MentionCluster[LinkedMention]

  type ExtractedSentence = Sentence with Extracted

  type SentencedResolvedDoc = Document with CorefResolved[LinkedMention] with Sentenced[ExtractedSentence]

  type document = SentencedResolvedDoc

  type mention = LinkedMention
}