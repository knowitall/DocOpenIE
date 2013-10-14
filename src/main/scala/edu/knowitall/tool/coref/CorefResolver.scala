package edu.knowitall.tool.coref

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.Mention
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.coref.Linked
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Extracted

trait CorefResolver[M <: Mention] {
  def resolve(doc: Document): Seq[MentionCluster[M]]
}

trait LinkedCorefResolver extends CorefResolver[Mention with Linked] {
  def resolve(doc: Document): Seq[MentionCluster[Mention with Linked]]
}

trait KBPRuleResolver extends LinkedCorefResolver {

  type LinkedMention = Mention with Linked

  type LinkedMentionCluster = MentionCluster[LinkedMention]

  type ExtractedSentence = Sentence with Extracted

  type SentencedResolvedDoc = Document with CorefResolved[LinkedMention] with Sentenced[ExtractedSentence]

  def resolve(doc: SentencedResolvedDoc): Seq[LinkedMentionCluster]
}