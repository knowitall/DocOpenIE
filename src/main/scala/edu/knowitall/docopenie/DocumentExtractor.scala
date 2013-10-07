package edu.knowitall.docopenie

trait SentenceExtractor extends (Sentence => Seq[Extraction])
trait Linker extends (MentionChain => Seq[Link])
trait Resolver extends (Document => Seq[MentionChain])
trait SentenceTagger extends (Sentence => TaggedSentence)

abstract class DocumentExtractor(
    sentenceExtractor: SentenceExtractor,
    linker: Linker,
    resolver: Resolver,
    tagger: SentenceTagger)
  extends (Document => ExtractedDocument) {

  def apply(document: Document): ExtractedDocument
//    val taggedSentences = document.sentences map tagger
//    val mentionChains = resolver(document)
//    val links = mentionChains.flatMap(linker)
//    val extractions = document.sentences flatMap sentenceExtractor
// TODO: Linker or Coref first? Chicken or Egg? Read NECo paper.
}