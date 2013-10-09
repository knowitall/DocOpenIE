package edu.knowitall.docopenie

trait SentenceTagger extends (Sentence => Set[SentenceTag])

trait SentenceExtractor extends (Sentence => Set[Extraction])

trait Resolver extends (Document => Set[CorefCluster])

abstract class DocumentExtractor(
    sentenceExtractor: SentenceExtractor,
    resolver: Resolver,
    tagger: SentenceTagger)
  extends (Document => ExtractedDocument)