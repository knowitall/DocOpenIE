package edu.knowitall.main

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.extraction.Extraction
import edu.knowitall.repr.extraction.ExtractionPart
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.bestentitymention.BestEntityMentionsFound

class KbpDocPrinter(out: java.io.PrintStream) {

  def print(kbpDoc: KbpDocument[Document with OpenIELinked with CorefResolved[Mention] with Sentenced[Sentence with OpenIEExtracted] with BestEntityMentionsFound]): Unit = {

    val KbpDocument(doc, docId) = kbpDoc

    out.println(docId)
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
  }

  def printCorefClusters(doc: Document with CorefResolved[Mention]): Unit = {

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
      for (extr <- sentence.extractions;
           a1offset = partOffset(extr.arg1, doc);
           reloffset = partOffset(extr.rel, doc);
           arg2 <- extr.arg2s) {
        val a2offset = partOffset(arg2, doc)
        out.println(f"\t($a1offset: ${extr.arg1.text})\t($reloffset: ${extr.rel.text})\t($a2offset: ${arg2.text})\tconf:${extr.confidence}%.03f")
      }
    }
    out.println()
  }

  def printLinks(doc: Document with OpenIELinked): Unit = {

    for (link <- doc.links) {
      link match {
        case f: FreeBaseLink =>
          val types = f.types.take(2).mkString(", ") + ", ..."
          out.println(f"(${f.offset})\t${f.name}\t${f.id}\t$types\t${f.score}%.02f=f(${f.docSimScore}%.02f, ${f.inlinks}%.02f, ${f.candidateScore}%.02f)")
        case _ => println(s"($link.offset)\t${link.toString}")
      }
    }
    out.println()
  }

  def partOffset(part: ExtractionPart, docSentence: DocumentSentence[Sentence with OpenIEExtracted]): Int = {
    docSentence.sentence.tokens(part.tokenIndices.head).offset + docSentence.offset
  }
}