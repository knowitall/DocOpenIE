package edu.knowitall.repr.document

import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Parsed
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.ner.StanfordSerializableNERAnnotated
import edu.knowitall.repr.ner.StanfordNERAnnotated
import edu.knowitall.repr.bestentitymention.BestEntityMention
import edu.knowitall.tool.ner.StanfordNERAnnotator
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.coref.CorefResolver
import edu.knowitall.tool.coref.StanfordCorefResolver
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.Stemmer
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.stem.{Lemmatized => TokenLemmatized}
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.tool.sentence.OpenIEExtractor
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.tool.link.OpenIELinked.ArgContext
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.bestentitymention.BestEntityMentionResolvedDocument
import edu.stanford.nlp.pipeline.Annotation

case class ExtractedDocument(
  override val text: String,
  val sentencesList: List[DocumentSentence[Sentence with OpenIEExtracted]],
  override val clusters: List[MentionCluster],
  override val annotationBytes: Array[Byte],
  override val links: List[FreeBaseLink],
  override val argContexts: List[ArgContext],
  override val bestEntityMentions: List[BestEntityMention],
  val docId: String)
extends Document(text)
  with OpenIELinked 
  with CorefResolved 
  with Sentenced[Sentence with OpenIEExtracted] 
  with StanfordSerializableNERAnnotated 
  with BestEntityMentionResolvedDocument 
  with DocId {
  
  override type B = BestEntityMention
  
  override def sentences = sentencesList.toStream
}

object ExtractedDocument {
  def convert(d: Document with OpenIELinked with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordNERAnnotated with BestEntityMentionResolvedDocument with DocId): ExtractedDocument = {
    ExtractedDocument(
      d.text,
      d.sentences.map(ds => ds.copy(sentence = ParsedSentence.convert(ds.sentence).asInstanceOf[Sentence with OpenIEExtracted])).toList,
      d.clusters.toList,
      StanfordSerializableNERAnnotated.annotationBytes(d.NERAnnotatedDoc),
      d.links.toList,
      d.argContexts.toList,
      d.bestEntityMentions.toList,
      d.docId)
  }
}



