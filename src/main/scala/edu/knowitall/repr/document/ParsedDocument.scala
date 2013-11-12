package edu.knowitall.repr.document

import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Parsed
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.coref.CorefResolver
import edu.knowitall.tool.coref.StanfordCorefResolver
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.Stemmer
import edu.knowitall.tool.stem.MorphaStemmer

case class ParsedDocument(
  override val text: String,
  override val sentences: Stream[DocumentSentence[Sentence with Parsed with Chunked with Lemmatized]],
  val clusters: Seq[MentionCluster],
  val docId: String) extends Document(text) with Sentenced[Sentence with Parsed with Chunked with Lemmatized] with CorefResolved with DocId

case class SentencedDocument(
  override val text: String,
  override val sentences: Stream[DocumentSentence[Sentence]],
  val docId: String
) extends Document(text) with Sentenced[Sentence] with DocId
  
case class DocumentParser(val parser: DependencyParser, chunker: Chunker, stemmer: Stemmer, resolver: CorefResolver) {
  
  type ParsedSentence = Sentence with Chunked with Parsed with Lemmatized
  
  def parse(d: SentencedDocument): ParsedDocument = {
    
    def parseSentence(s: Sentence): ParsedSentence = {
      val parse = parser(s.text)
      val postokens = parse.nodes.toSeq
      val chunkTokens = chunker.chunkPostagged(postokens)
      new Sentence(s.text) with Parsed with Chunked with Lemmatized {
        override val lemmatizedTokens = chunkTokens map stemmer.stemToken
        private val dgraphString = DependencyGraph.stringFormat.write(parse)
        override def dgraph = DependencyGraph.stringFormat.read(dgraphString)
        override val tokens = chunkTokens
      }
    }
    
    val parsedSentences = d.sentences.map { case DocumentSentence(sentence, offset) =>
      DocumentSentence(parseSentence(sentence), offset)
    }
    val clusters = resolver.resolve(d)
    
    ParsedDocument(d.text, parsedSentences, clusters, d.docId)
  }
}

object DocumentParser {
  
  private lazy val defaultParser = new ClearParser()
  private lazy val defaultChunker = new OpenNlpChunker()
  private lazy val defaultStemmer = new MorphaStemmer()
  private lazy val defaultResolver = new StanfordCorefResolver()
  
  lazy val defaultInstance: DocumentParser = DocumentParser(defaultParser, defaultChunker, defaultStemmer, defaultResolver)
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