package edu.knowitall.repr.document

import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Parsed
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.ner.StanfordSerializableNERAnnotated
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
import edu.stanford.nlp.pipeline.Annotation

case class ParsedDocument(
  override val text: String,
  val sentencesList: List[DocumentSentence[Sentence with OpenIEExtracted]],
  val clusters: List[MentionCluster],
  override val annotationBytes: Array[Byte],
  val docId: String)
extends
  Document(text) with
  Sentenced[Sentence with OpenIEExtracted] with
  CorefResolved with
  StanfordSerializableNERAnnotated with
  DocId
{

  override def sentences = sentencesList.toStream
}

case class ParsedSentence(
  override val text: String,
  override val lemmatizedTokens: List[TokenLemmatized[ChunkedToken]],
  private val dgraphString: String,
  val tokenStrings: List[String]
) extends Sentence(text) with OpenIEExtractor {

  override def dgraph: DependencyGraph = DependencyGraph.stringFormat.read(dgraphString)

  override def tokens = tokenStrings.map(ChunkedToken.stringFormat.read)
}

case class SentencedDocument(
  override val text: String,
  override val sentences: Stream[DocumentSentence[Sentence]],
  val docId: String
) extends Document(text) with Sentenced[Sentence] with DocId

case class DocumentParser(val parser: DependencyParser, chunker: Chunker, stemmer: Stemmer, resolver: CorefResolver, nerTagger: StanfordNERAnnotator) {

  def parse(d: SentencedDocument): ParsedDocument = {

    def parseSentence(s: Sentence): Sentence with OpenIEExtracted = {
      val parse = parser(s.text)
      val postokens = parse.nodes.toSeq
      val chunkTokens = chunker.chunkPostagged(postokens)
      val lemmatizedTokens = (chunkTokens map stemmer.stemToken).toList
      val dgraphString = DependencyGraph.stringFormat.write(parse)
      val tokenStrings = chunkTokens.toList.map(ChunkedToken.stringFormat.write)
      ParsedSentence(s.text, lemmatizedTokens, dgraphString, tokenStrings)
    }

    val parsedSentences = d.sentences.map { case DocumentSentence(sentence, offset) =>
      DocumentSentence(parseSentence(sentence), offset)
    }
    val clusters = resolver.resolve(d)
    val nerAnnotation = nerTagger.annotate(d)
    val annotationBytes = StanfordSerializableNERAnnotated.annotationBytes(nerAnnotation)

    ParsedDocument(d.text, parsedSentences.toList, clusters.toList, annotationBytes, d.docId)
  }
}

object DocumentParser {

  private lazy val defaultParser = new ClearParser()
  private lazy val defaultChunker = new OpenNlpChunker()
  private lazy val defaultStemmer = new MorphaStemmer()
  private lazy val defaultResolver = new StanfordCorefResolver()
  private lazy val defaultNERTagger = new StanfordNERAnnotator()

  lazy val defaultInstance: DocumentParser = DocumentParser(defaultParser, defaultChunker, defaultStemmer, defaultResolver, defaultNERTagger)
}

object KbpDocumentSentencer {

  import edu.knowitall.prep._
  import edu.knowitall.prep.util.LineReader
  import java.io.File
  import edu.knowitall.common.Resource.using

  val kbpSentencer = Sentencer.defaultInstance

  def loadKbpDoc(docFile: File): Seq[(File, KbpRawDoc)] = {
    val docs = using(LineReader.fromFile(docFile, "UTF8")) { lineReader =>
      val docSplitter = new DocSplitter(lineReader)
      docSplitter.toList
    }

    if (docs.size != 1) System.err.println(s"Warning, loaded ${docs.size} docs from ${docFile.getName()}")
    docs.map((docFile, _))
  }

  def loadKbpDocs(path: String): Seq[(File, KbpRawDoc, KbpProcessedDoc)] = {

    val docPath = new File(path)

    val docFiles = docPath.listFiles().filter(_.getName().endsWith("sgm"))

    val rawDocs = docFiles flatMap loadKbpDoc

    rawDocs map { case (file, doc) => (file, doc, processDoc(file, doc)) }
  }

  def loadSentencedDocs(path: String) = {
    loadKbpDocs(path).toSeq.map { case (file, rawDoc, procDoc) =>
      val text = rawDoc.getString
      val kbpSentences = kbpSentencer.convertToSentences(procDoc)
      val sentences = kbpSentences.toStream.map { ks =>
        val sent = new Sentence(ks.text)
        DocumentSentence(sent, ks.offset)
      }
      SentencedDocument(text, sentences, procDoc.extractDocId.get)
    }
  }

  def processDoc(file: File, rawDoc: KbpRawDoc): KbpProcessedDoc = {
    KbpWebDocProcessor.process(rawDoc) match {
      case Some(procDoc) => procDoc
      case None => {
        throw new RuntimeException(file.getName())
      }
    }
  }
}