package edu.knowitall.main

import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.link.Link
import edu.knowitall.tool.document.OpenIEBaselineExtractor
import edu.knowitall.tool.document.OpenIEDocumentExtractor
import edu.knowitall.tool.document.OpenIENoCorefDocumentExtractor

object LinkDiffPrinter extends App {

  import DocOpenIEMain.loadSentencedDocs

  val sentencedDocuments = loadSentencedDocs(args(0))//.filter(_.docId.contains("NYT_ENG_20020724.0283.LDC2007T07"))

  val baseLineExtractor = new OpenIEBaselineExtractor()
  val fullExtractor = new OpenIEDocumentExtractor()

  val baseLineExtracted = sentencedDocuments map (kd => kd.copy(doc=baseLineExtractor.extract(kd.doc)))
  val fullExtracted = sentencedDocuments map (kd => kd.copy(doc=fullExtractor.extract(kd.doc)))

  val psout = new java.io.PrintStream(args(1))

  val diffPrinter = new LinkDiffPrinter(psout)
  psout.println(diffPrinter.columnHeaderString)
  baseLineExtracted.zip(fullExtracted).map { case (base, full) =>
    diffPrinter.print(base, full)
  }
  psout.flush()
  psout.close()
}

class LinkDiffPrinter(out: java.io.PrintStream) {

  type LinkedDocument = KbpDocument[_ <: Document with Sentenced[_ <: Sentence] with OpenIELinked]

  // links are distinct given an offset, the text they linked to, and their link ID.
  private def linkKey(l: FreeBaseLink) = (l.offset, l.text, l.id)

  def print(oldDoc: LinkedDocument, newDoc: LinkedDocument): Unit = {

    require(oldDoc.docId.equals(newDoc.docId), "Link diff should be for the same doc.")

    val oldLinks = oldDoc.doc.links.map(l => (linkKey(l), l)).toMap
    val newLinks = newDoc.doc.links.map(l => (linkKey(l), l)).toMap

    val oldDiff = (oldLinks -- newLinks.keys).toSeq
    val newDiff = (newLinks -- oldLinks.keys).toSeq

    // old links are paired with "true", new links "false".
    val combined = (oldDiff.map((true, _)) ++ newDiff.map((false, _)))
    // sort by offset
    val sorted = combined.toSeq.sortBy(_._2._2.offset)

    sorted foreach { case (oldFlag, (key, link)) =>
      out.println(diffString(oldFlag, link, { if (oldFlag) oldDoc else newDoc }))
    }
  }

  val columnHeaders = Seq("SOURCE", "OFFSET", "ORIGINAL TEXT", "WIKI TITLE", "FB ID", "TYPES", "COMBINED SCORE", "DOC SIM SCORE", "INLINKS", "CROSSWIKIS SCORE", "CONTEXT CLUSTER MENTIONS", "CONTEXT SIZE", "CONTEXT", "DOC ID")
  val columnHeaderString = columnHeaders.mkString("\t")

  def diffString(old: Boolean, link: FreeBaseLink, kbpDoc: LinkedDocument): String = {

    val argContexts = kbpDoc.doc.argContexts.toSeq.filter { case (arg, _, ctxt) =>
      val chStartMatch = link.offset == arg.offset + ctxt.source.offset
      val chEndMatch = link.offset + link.text.length == arg.offset + ctxt.source.offset + arg.text.length
      val textMatch = link.text == arg.text
      chStartMatch && chEndMatch && textMatch
    }
    require(argContexts.size == 1, s"${argContexts.size} links found, expected one.")

    val context = argContexts.headOption

    val contextString = context.map(_._3.fullText.mkString("[", ", ", "]")).getOrElse("CONTEXT NOT FOUND")

    val contextMentions = context.map { case (_, _, ctxt) =>
      val clusters = ctxt.clusters
      val texts = clusters.map { c =>
        c.mentions.map { case m: Mention =>
          s"(${m.offset}) ${m.text}"
        }
      }
      texts.mkString("[", "] [", "]")
    }.getOrElse("[]")
    val contextSize = context.map(_._3.size).getOrElse(0)
    val diffType = if (old) "BASELINE" else "NEW"
    val fields = Seq(diffType, linkString(link), contextMentions, contextSize, contextString, kbpDoc.docId)
    fields.mkString("\t")
  }

  def linkString(l: Link): String = {

    def fmt(d: Double) = "%.02f" format d

    l match {
        case f: FreeBaseLink =>
          val types = f.types.take(2).mkString(", ") + { if (f.types.size > 2) ", ..." else "" }
          val fields = Seq(f.offset, f.text, f.name, f.id, types, fmt(f.score), fmt(f.docSimScore), fmt(f.inlinks), fmt(f.candidateScore))
          fields.mkString("\t")
        case _ => s"(l.offset)\t${l.toString}"
    }
  }
}