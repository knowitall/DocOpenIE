package edu.knowitall.tool.document

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Extracted
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.extraction.Extraction
import edu.knowitall.repr.sentence.Parsed
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.coref.StanfordCorefResolver
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.browser.entity.EntityLinker
import java.io.File
import edu.knowitall.tool.bestentitymention.BestEntityMentionsFound
import edu.knowitall.tool.bestentitymention.BestEntityMentionFinderOriginalAlgorithm

class OpenIEDocumentExtractor {

  type InputDoc = Document with Sentenced[_ <: Sentence]
  type OutputDoc = Document with OpenIELinked with CorefResolved[Mention] with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound
  
  val parser = new ClearParser()
  val chunker = new OpenNlpChunker()
  val stemmer = new MorphaStemmer()
  val entityLinker = new EntityLinker(new File("/scratch/"))
  val stanfordResolver = new StanfordCorefResolver()
  val bestEntityMentionFinderAlgorithm = new BestEntityMentionFinderOriginalAlgorithm()


  def prepSentence(s: Sentence): Sentence with OpenIEExtracted = {
    val parse = parser(s.text)
    val postokens = parse.nodes.toSeq
    val chunkTokens = chunker.chunkPostagged(postokens)
    new Sentence(s.text) with OpenIEExtracted with Parsed with Chunked with Lemmatized {
      override val lemmatizedTokens = chunkTokens map stemmer.stemToken
      override val dgraph = parse
      override val tokens = chunkTokens
    }
  }

  def extract(d: Document with Sentenced[_ <: Sentence]): Document with OpenIELinked with CorefResolved[Mention] with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound = {

    val preppedSentences = d.sentences.map { case DocumentSentence(sentence, offset) =>
      DocumentSentence(prepSentence(sentence), offset)
    }

    new Document(d.text) with OpenIELinked with CorefResolved[Mention] with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound {
      val clusters = stanfordResolver.resolve(d)
      val sentences = preppedSentences
      val linker = entityLinker
      val bestEntityMentionFinder = bestEntityMentionFinderAlgorithm
    }
  }
}

class OpenIENoCorefDocumentExtractor {

  type InputDoc = Document with Sentenced[_ <: Sentence]
  type OutputDoc = Document with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound
  
  val parser = new ClearParser()
  val chunker = new OpenNlpChunker()
  val stemmer = new MorphaStemmer()
  val entityLinker = new EntityLinker(new File("/scratch/"))
  val bestEntityMentionFinderAlgorithm = new BestEntityMentionFinderOriginalAlgorithm()


  def prepSentence(s: Sentence): Sentence with OpenIEExtracted = {
    val parse = parser(s.text)
    val postokens = parse.nodes.toSeq
    val chunkTokens = chunker.chunkPostagged(postokens)
    new Sentence(s.text) with OpenIEExtracted with Parsed with Chunked with Lemmatized {
      override val lemmatizedTokens = chunkTokens map stemmer.stemToken
      override val dgraph = parse
      override val tokens = chunkTokens
    }
  }

  def extract(d: Document with Sentenced[_ <: Sentence]): Document with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound = {

    val preppedSentences = d.sentences.map { case DocumentSentence(sentence, offset) =>
      DocumentSentence(prepSentence(sentence), offset)
    }

    new Document(d.text) with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound {
      val sentences = preppedSentences
      val linker = entityLinker
      val bestEntityMentionFinder = bestEntityMentionFinderAlgorithm
    }
  }
}

class OpenIEBaselineExtractor {

  type InputDoc = Document with Sentenced[_ <: Sentence]
  type OutputDoc = Document with OpenIELinked with Sentenced[Sentence with OpenIEExtracted]  
  
  val parser = new ClearParser()
  val chunker = new OpenNlpChunker()
  val stemmer = new MorphaStemmer()
  val entityLinker = new EntityLinker(new File("/scratch/"))

  def prepSentence(s: Sentence): Sentence with OpenIEExtracted = {
    val parse = parser(s.text)
    val postokens = parse.nodes.toSeq
    val chunkTokens = chunker.chunkPostagged(postokens)
    new Sentence(s.text) with OpenIEExtracted with Parsed with Chunked with Lemmatized {
      override val lemmatizedTokens = chunkTokens map stemmer.stemToken
      override val dgraph = parse
      override val tokens = chunkTokens
    }
  }

  def extract(d: Document with Sentenced[_ <: Sentence]): Document with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] = {

    val preppedSentences = d.sentences.map { case DocumentSentence(sentence, offset) =>
      DocumentSentence(prepSentence(sentence), offset)
    }

    new Document(d.text) with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] {
      val sentences = preppedSentences
      val linker = entityLinker
    }
  }
}