package edu.knowitall.tool.document

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocId
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Extracted
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.extraction.Extraction
import edu.knowitall.repr.ner.StanfordNERAnnotated
import edu.knowitall.repr.ner.StanfordSerializableNERAnnotated
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
import edu.knowitall.tool.link.OpenIELinker
import edu.knowitall.browser.entity.EntityLinker
import java.io.File
import edu.knowitall.tool.bestentitymention.BestEntityMentionsFound
import edu.knowitall.tool.bestentitymention.BestEntityMentionFinderOriginalAlgorithm
import edu.knowitall.repr.link.LinkedDocument
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.bestentitymention.BestEntityMentionResolvedDocument
import edu.knowitall.repr.bestentitymention.BestEntityMention
import edu.knowitall.repr.coref.MentionCluster

trait OpenIEDocumentExtractor {

  import OpenIEDocumentExtractor.BaseInputDoc
  import OpenIEDocumentExtractor.BaseOutputDoc

  type InputDoc <: BaseInputDoc
  type OutputDoc <: BaseOutputDoc

  def extract(doc: InputDoc): OutputDoc
}

object OpenIEDocumentExtractor {

  type BaseSentence = Sentence with OpenIEExtracted

  type BaseInputDoc = Document with Sentenced[_ <: BaseSentence] with DocId
  type BaseOutputDoc = Document with OpenIELinked with Sentenced[_ <: BaseSentence] with DocId


  lazy val entityLinker = new EntityLinker(new File("/scratch/"))
  lazy val bestEntityMentionFinderAlgorithm = new BestEntityMentionFinderOriginalAlgorithm()

//  def extractSentence(s: BaseSentence): Sentence with OpenIEExtracted = {
//    new Sentence(s.text) with OpenIEExtracted {
//      override val lemmatizedTokens = s.lemmatizedTokens
//      override val dgraph = s.dgraph
//      override val tokens = s.tokens
//    }
//  }
//
//  def extractAllSentences(d: BaseInputDoc) = {
//    d.sentences.map { case DocumentSentence(sentence, offset) =>
//      DocumentSentence(extractSentence(sentence), offset)
//    }
//  }
}

class OpenIENoCorefDocumentExtractor extends OpenIEDocumentExtractor {

  import OpenIEDocumentExtractor._

  type InputDoc = Document with Sentenced[BaseSentence] with StanfordNERAnnotated with DocId
  type OutputDoc = Document with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound with DocId

  override def extract(d: InputDoc): OutputDoc = {

    new Document(d.text) with OpenIELinker with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound with StanfordNERAnnotated with DocId {
      val sentences = d.sentences
      val linker = entityLinker
      val NERAnnotatedDoc = d.NERAnnotatedDoc
      val bestEntityMentionFinder = bestEntityMentionFinderAlgorithm
      val docId = d.docId
    }
  }
}

class OpenIEBaselineExtractor extends OpenIEDocumentExtractor {

  import OpenIEDocumentExtractor._

  type InputDoc = Document with Sentenced[BaseSentence] with DocId
  type OutputDoc = Document with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] with DocId

  def extract(d: InputDoc): OutputDoc = {

    new Document(d.text) with OpenIELinker with Sentenced[Sentence with OpenIEExtracted] with DocId {
      val sentences = d.sentences
      val linker = entityLinker
      val docId = d.docId
    }
  }
}

class OpenIECorefExpandedDocumentExtractor(val debug: Boolean = false) extends OpenIEDocumentExtractor {

  import OpenIEDocumentExtractor._

  type InputDoc = Document with Sentenced[BaseSentence] with CorefResolved with StanfordNERAnnotated with DocId
  type OutputDoc = Document with OpenIELinked with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordNERAnnotated with BestEntityMentionResolvedDocument with DocId

  def getUniqueLinksInCluster(cluster: MentionCluster, links : Seq[FreeBaseLink]): Seq[FreeBaseLink] = {
    var linksInCluster = Seq[FreeBaseLink]()
    for(m <- cluster.mentions){
      for(link <- links){
        if(link.offset == m.offset){
          linksInCluster = linksInCluster :+ link
        }
      }
    }
    linksInCluster.groupBy(f => f.id).values.map(f => f.head).toSeq
  }

  def getUniqueBestEntityMentionsInCluster(cluster: MentionCluster, bestEntityMentions: Seq[BestEntityMention]): Seq[BestEntityMention] = {
    var bestEntityMentionsInCluster = Seq[BestEntityMention]()
    for(m <- cluster.mentions){
      for(bem <- bestEntityMentions){
        if(bem.offset == m.offset){
          bestEntityMentionsInCluster = bestEntityMentionsInCluster :+ bem
        }
      }
    }
    bestEntityMentionsInCluster.groupBy(f => f.bestEntityMention).values.map(f => f.head).toSeq
  }

  def makeNewBestEntityMentionsForPronounsInCluster(cluster: MentionCluster, bestName: String) = {
    for(m <- cluster.mentions; if m.isPronoun) yield BestEntityMention(m.text,m.offset,bestName)
  }

  def extract(d: InputDoc): OutputDoc = {

    val doc  = new Document(d.text) with OpenIELinker with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordNERAnnotated with BestEntityMentionsFound {
      type M = Mention
      val clusters = d.clusters
      val sentences = d.sentences
      val linker = entityLinker
      val NERAnnotatedDoc = d.NERAnnotatedDoc
      val bestEntityMentionFinder = bestEntityMentionFinderAlgorithm
    }
    if(debug){
	    println("Document : " + d.sentences.head)
	    println("All links in Doc: ")
	    for(link <- doc.links){
	      println(link.offset + "\t" + link.name)
	    }
	    println("All BEMS in Doc: ")
	    for(bem <- doc.bestEntityMentions){
	      println(bem.offset + "\t" + bem.bestEntityMention)
	    }
    }
    var corefExpandedBestEntityMentions = doc.bestEntityMentions
    var clusterIndex =1
    for(cluster <- doc.clusters){
      val mentions = cluster.mentions
      val links = getUniqueLinksInCluster(cluster,doc.links)
      val bestEntityMentions = getUniqueBestEntityMentionsInCluster(cluster, doc.bestEntityMentions)
      var bestName :Option[String] = None
      if(debug){
	      println("Cluster number " + clusterIndex)
	      println("Mentions: ")
	      for(mention <- mentions){
	        println(mention.offset + "\t" + mention.text + "\t" + mention.isPronoun)
	      }
	      println("Unique links: ")
	      for(l <- links){
	        println(l.offset + "\t" + l.name)
	      }
	      println("Unique BEMS: ")
	      for(bem <- bestEntityMentions){
	        println(bem.offset + "\t" + bem.bestEntityMention)
	      }
      }
      if(links.length ==1)  {
        bestName = Some(links.head.name)
      }
      else if(bestEntityMentions.length ==1) {
        bestName = Some(bestEntityMentions.head.bestEntityMention)
      }
      if(bestName.isDefined){
        val newBestEntityMentions = makeNewBestEntityMentionsForPronounsInCluster(cluster,bestName.get)
        corefExpandedBestEntityMentions = corefExpandedBestEntityMentions ++ newBestEntityMentions
      }
      clusterIndex += 1
    }
    if(debug){
	    println("New BEMS:")
	    for(newMention <- corefExpandedBestEntityMentions){
	      if(doc.bestEntityMentions.forall(p => p.offset != newMention.offset)){
	        println(newMention.offset +"\t" + newMention.bestEntityMention)
	      }
	    }
    }
    val newDoc = new Document(d.text) with OpenIELinked with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordSerializableNERAnnotated with BestEntityMentionResolvedDocument with DocId {
      type M = Mention
      type B = BestEntityMention
      override val argContexts = doc.argContexts.toList
      val links = doc.links.toList
      val clusters = doc.clusters.toList
      val sentencesList = doc.sentences.toList
      def sentences = sentencesList.toStream
      val bestEntityMentions = corefExpandedBestEntityMentions.toList
      val docId = d.docId
      val annotationBytes = StanfordSerializableNERAnnotated.annotationBytes(d.NERAnnotatedDoc)
    }
    newDoc
  }
}
