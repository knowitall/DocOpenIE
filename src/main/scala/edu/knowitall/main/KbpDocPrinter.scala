package edu.knowitall.main

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocId
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.extraction.Extraction
import edu.knowitall.repr.extraction.ExtractionPart
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.link.Link
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.bestentitymention.BestEntityMentionsFound

/**
 * Code for producing human readable sample output.
 */
class KbpDocPrinter(out: java.io.PrintStream) {

  def print(doc: Document with OpenIELinked with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound with DocId): Unit = {

    out.println(doc.docId)
    out.println("Number of sentences: " + doc.sentences.size)
    out.println("Number of extractions: " + doc.sentences.flatMap(_.sentence.extractions).size)
    out.println("Number of FreeBase links: " + doc.links.size)
    out.println()
    out.println("Text sample:")
    out.println(doc.text.take(300) + "...")
    out.println()
    out.println("Extracted Sentences:")
    printSentences(doc)
    out.println("Links:")
    printLinks(doc)
    out.println("Coref Clusters:")
    printCorefClusters(doc)
    out.println("Best Entity Mentions:")
    printEntityMentions(doc)
  }

  def printEntityMentions(doc: Document with BestEntityMentionsFound): Unit = {
    for( bme <-doc.bestEntityMentions){
      out.println(bme.offset + "\t" + bme.text + "\t" + bme.bestEntityMention)
    }
  }

  def printCorefClusters(doc: Document with CorefResolved): Unit = {

    def pm(m: Mention) = s"(${m.offset}) ${m.text}"

    val clusters = doc.clusters.sortBy(-_.mentions.size)

    clusters.foreach { mc =>
      val filt = mc.mentions.filterNot(_ == mc.best)
      out.println(pm(mc.best) + "\t" + filt.map(pm).mkString(", "))
    }
  }

  def printSentences(doc: Document with Sentenced[Sentence with OpenIEExtracted]): Unit = {

    val sents = doc.sentences

    for (doc @ DocumentSentence(sentence, offset) <- sents) {
      out.println(s"($offset) ${sentence.text}")
      for (extr <- sentence.extractions) {
        val a1offset = partOffset(extr.arg1, doc)
        val reloffset = partOffset(extr.rel, doc)
        val a2offset = partOffset(extr.arg2, doc)
        out.println(f"\t($a1offset: ${extr.arg1.text})\t($reloffset: ${extr.rel.text})\t($a2offset: ${extr.arg2.text})\tconf:${extr.confidence}%.03f")
      }
    }
    out.println()
  }

  def linkString(l: Link): String = {
    l match {
        case f: FreeBaseLink =>
          val types = f.types.take(2).mkString(", ") + ", ..."
          f"(${f.offset})\t${f.name}\t${f.id}\t$types\t${f.score}%.02f=f(${f.docSimScore}%.02f, ${f.inlinks}%.02f, ${f.candidateScore}%.02f)"
        case _ => s"(l.offset)\t${l.toString}"
    }
  }

  def printLinks(doc: Document with OpenIELinked): Unit = {

    for (link <- doc.links) {
      out.println(linkString(link))
    }
    out.println()
  }

  def partOffset(part: ExtractionPart, docSentence: DocumentSentence[Sentence with OpenIEExtracted]): Int = {
    docSentence.sentence.tokens(part.tokenIndices.head).offset + docSentence.offset
  }
}

object KbpDocPrinter extends App {
  
  import java.io.File
  
  val extractedDocuments = new File(args(0)).listFiles.map(FullDocSerializer.deserializeFromFile)
  
  val docPrinter = new KbpDocPrinter(System.out)
     
  extractedDocuments foreach docPrinter.print
  
}


