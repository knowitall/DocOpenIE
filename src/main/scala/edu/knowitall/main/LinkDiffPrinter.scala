package edu.knowitall.main

import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocumentParser
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.document.DocId
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.document.KbpDocumentSentencer
import edu.knowitall.tool.document.OpenIEBaselineExtractor
import edu.knowitall.tool.document.OpenIEDocumentExtractor
import edu.knowitall.tool.document.OpenIENoCorefDocumentExtractor
import edu.knowitall.tool.document.OpenIECorefExpandedDocumentExtractor

object LinkDiffPrinter extends App {

  val sentencedDocuments = KbpDocumentSentencer.loadSentencedDocs(args(0))

  val parsedDocuments = sentencedDocuments.map(DocumentParser.defaultInstance.parse)
  
  val baseline = new OpenIEBaselineExtractor()
  val rules = new OpenIENoCorefDocumentExtractor()
  val coref = new OpenIECorefExpandedDocumentExtractor()
  
  val baselineExtracted = parsedDocuments.map(baseline.extract)
  val rulesExtracted = parsedDocuments.map(rules.extract)
  val corefExtracted = parsedDocuments.map(coref.extract)

  val zippedDocs = baselineExtracted.zip(rulesExtracted).zip(corefExtracted)

  val psout = new java.io.PrintStream(args(1))

  val diffPrinter = new LinkDiffPrinter(psout)
  psout.println(diffPrinter.columnHeaderString)
  zippedDocs.foreach { case ((baseDoc, rulesDoc), corefDoc) =>
    diffPrinter.printAll("BASELINE", baseDoc)
    diffPrinter.printAll("RULES", rulesDoc)
    diffPrinter.printAll("COREF", corefDoc)
  }
  psout.flush()
  psout.close()
}

class LinkDiffPrinter(out: java.io.PrintStream) {

  type LKBPDoc = Document with Sentenced[_ <: Sentence] with OpenIELinked with DocId

  // links are distinct given an offset, the text they linked to, and their link ID.
  private def linkKey(l: FreeBaseLink) = (l.offset, l.text, l.id)

  def printDiff(oldDoc: LKBPDoc, newDoc: LKBPDoc): Unit = {

    require(oldDoc.docId.equals(newDoc.docId), "Link diff should be for the same doc.")

    val oldLinks = oldDoc.links.map(l => (linkKey(l), l)).toMap
    val newLinks = newDoc.links.map(l => (linkKey(l), l)).toMap

    val oldDiff = (oldLinks -- newLinks.keys).toSeq
    val newDiff = (newLinks -- oldLinks.keys).toSeq

    // old links are paired with "true", new links "false".
    val combined = (oldDiff.map((true, _)) ++ newDiff.map((false, _)))
    // sort by offset
    val sorted = combined.toSeq.sortBy(_._2._2.offset)

    sorted foreach { case (oldFlag, (key, link)) =>
      val name = if (oldFlag) "BASELINE" else "NEW"
      out.println(linkString(name, link, { if (oldFlag) oldDoc else newDoc }))
    }
  }

  def printAll(runName: String, doc: LKBPDoc): Unit = {
    doc.links foreach { link =>
      out.println(linkString(runName, link, doc))
    }
  }

  val columnHeaders = Seq("SOURCE", "OFFSET", "ORIGINAL TEXT", "CLEANED TEXT", "WIKI TITLE", "FB ID", "TYPES", "COMBINED SCORE", "DOC SIM SCORE", "INLINKS", "CROSSWIKIS SCORE", "CONTEXT CLUSTER MENTIONS", "CONTEXT SIZE", "CONTEXT", "DOC ID")
  val columnHeaderString = columnHeaders.mkString("\t")

  def linkString(name: String, link: FreeBaseLink, kbpDoc: LKBPDoc): String = {

    val argContexts = kbpDoc.argContexts.toSeq.filter { ac =>
      val chStartMatch = link.offset == ac.arg.offset + ac.source.offset
      val chEndMatch = link.offset + link.text.length == ac.arg.offset + ac.source.offset + ac.arg.text.length
      val textMatch = link.text == ac.arg.text
      chStartMatch && chEndMatch && textMatch
    }
    require(argContexts.size == 1, s"${argContexts.size} links found, expected one.")

    val context = argContexts.headOption

    val contextString = context.map(_.fullText.mkString("[", ", ", "]")).getOrElse("CONTEXT NOT FOUND")

    val contextMentions = context.map { ac =>
      val clusters = ac.clusters
      val texts = clusters.map { c =>
        c.mentions.map { case m: Mention =>
          s"(${m.offset}) ${m.text}"
        }
      }
      texts.mkString("[", "] [", "]")
    }.getOrElse("[]")
    val contextSize = context.map(_.size).getOrElse(0)
    val cleanString = context.map(_.cleanArg).getOrElse("NA")
    val fields = Seq(name) ++ linkFields(link, cleanString) ++ Seq(contextMentions, contextSize.toString, contextString, kbpDoc.docId)
    fields.map(EvaluationPrinter.clean).mkString("\t")
  }

  def linkFields(f: FreeBaseLink, cleanString: String): Seq[String] = {

    def fmt(d: Double) = "%.02f" format d
    val types = f.types.take(2).mkString(", ") + { if (f.types.size > 2) ", ..." else "" }
    val fields = Seq(f.offset.toString, f.text, cleanString, f.name, f.id, types, fmt(f.score), fmt(f.docSimScore), fmt(f.inlinks), fmt(f.candidateScore))
    fields
  }
}