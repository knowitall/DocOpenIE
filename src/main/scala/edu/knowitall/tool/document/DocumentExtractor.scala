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
import edu.knowitall.tool.bestmention.BestMentionsFound
import edu.knowitall.tool.bestmention.BestMentionFinderOriginalAlgorithm
import edu.knowitall.repr.link.LinkedDocument
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.bestmention._
import edu.knowitall.repr.bestmention.EntityType._
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
  lazy val bestMentionFinderAlgorithm = new BestMentionFinderOriginalAlgorithm()

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
  type OutputDoc = Document with OpenIELinked with Sentenced[Sentence with OpenIEExtracted] with BestMentionsFound with DocId

  override def extract(d: InputDoc): OutputDoc = {

    new Document(d.text) with OpenIELinker with Sentenced[Sentence with OpenIEExtracted] with BestMentionsFound with StanfordNERAnnotated with DocId {
      val sentences = d.sentences
      val linker = entityLinker
      val NERAnnotatedDoc = d.NERAnnotatedDoc
      val bestMentionFinder = bestMentionFinderAlgorithm
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
  type OutputDoc = Document with OpenIELinked with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordNERAnnotated with BestMentionResolvedDocument with DocId

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

  def getUniquebestMentionsInCluster(cluster: MentionCluster, bestMentions: Seq[ResolvedBestMention]): Seq[ResolvedBestMention] = {
    var bestMentionsInCluster = Seq[ResolvedBestMention]()
    for(m <- cluster.mentions){
      for(bem <- bestMentions){
        if(bem.offset == m.offset){
          bestMentionsInCluster = bestMentionsInCluster :+ bem
        }
      }
    }
    bestMentionsInCluster.groupBy(f => f.bestMention).values.map(f => f.head).toSeq
  }

  def newCorefMentions(cluster: MentionCluster, bestMention: ResolvedBestMention) = {
    for(m <- cluster.mentions; if m.isPronoun) yield {
      val target = Entity(m.text,m.offset,m.text, bestMention.target.entityType)
      if (bestMention.isInstanceOf[FullResolvedBestMention]) {
        val fbm = bestMention.asInstanceOf[FullResolvedBestMention]
        CorefFullResolvedBestMention(target, fbm.bestEntity, cluster, bestMention.candidateCount)
      } else {
        CorefResolvedBestMention(target, bestMention.bestMention, cluster, bestMention.candidateCount)
      }
    }
  }
  
  def newLinkMentions(cluster: MentionCluster, link: FreeBaseLink) = {
    for(m <- cluster.mentions; if m.isPronoun) yield {
      LinkResolvedBestMention(m, link, cluster, 1)
    }
  }
  


  def extract(d: InputDoc): OutputDoc = {

    val doc  = new Document(d.text) with OpenIELinker with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordNERAnnotated with BestMentionsFound {
      type M = Mention
      val clusters = d.clusters
      val sentences = d.sentences
      val linker = entityLinker
      val NERAnnotatedDoc = d.NERAnnotatedDoc
      val bestMentionFinder = bestMentionFinderAlgorithm
    }
    if(debug){
	    println("Document : " + d.sentences.head)
	    println("All links in Doc: ")
	    for(link <- doc.links){
	      println(link.offset + "\t" + link.name)
	    }
	    println("All BEMS in Doc: ")
	    for(bem <- doc.bestMentions){
	      println(bem.offset + "\t" + bem.bestMention)
	    }
    }
    var corefExpandedbestMentions = doc.bestMentions
    var clusterIndex =1
    for(cluster <- doc.clusters){
      val mentions = cluster.mentions
      val links = getUniqueLinksInCluster(cluster,doc.links)
      val bestMentions = getUniquebestMentionsInCluster(cluster, doc.bestMentions)
      var best: Option[CorefResolvedBestMention] = None
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
	      for(bem <- bestMentions){
	        println(bem.offset + "\t" + bem.bestMention)
	      }
      }
      if(links.length ==1)  {
        val newbestMentions = newLinkMentions(cluster,links.head)
        corefExpandedbestMentions = corefExpandedbestMentions ++ newbestMentions
      }
      else if(bestMentions.length ==1) {
        val newbestMentions = newCorefMentions(cluster, bestMentions.head)
        corefExpandedbestMentions = corefExpandedbestMentions ++ newbestMentions
      }
      clusterIndex += 1
    }
    if(debug){
	    println("New BEMS:")
	    for(newMention <- corefExpandedbestMentions){
	      if(doc.bestMentions.forall(p => p.offset != newMention.offset)){
	        println(newMention.offset +"\t" + newMention.bestMention)
	      }
	    }
    }
    val newDoc = new Document(d.text) with OpenIELinked with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordNERAnnotated with BestMentionResolvedDocument with DocId {
      type M = Mention
      type B = ResolvedBestMention
      override val argContexts = doc.argContexts
      val links = doc.links
      val clusters = doc.clusters
      val sentences = doc.sentences
      val bestMentions = corefExpandedbestMentions
      val docId = d.docId
      val NERAnnotatedDoc = d.NERAnnotatedDoc
    }
    newDoc
  }
}
