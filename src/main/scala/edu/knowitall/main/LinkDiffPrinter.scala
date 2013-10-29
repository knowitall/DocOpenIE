package edu.knowitall.main

import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.link.Link
import edu.knowitall.tool.document.OpenIEBaselineExtractor
import edu.knowitall.tool.document.OpenIEDocumentExtractor

object LinkDiffPrinter extends App {
  
  type LinkedDocument = Document with Sentenced[_ <: Sentence] with OpenIELinked
  
  import DocOpenIEMain.loadSentencedDocs
  
  val sentencedDocuments = loadSentencedDocs(args(0))
  
  val baseLineExtractor = new OpenIEBaselineExtractor()
  val fullExtractor = new OpenIEDocumentExtractor()
  
  val baseLineExtracted = sentencedDocuments map (kd => kd.copy(doc=baseLineExtractor.extract(kd.doc)))
  val fullExtracted = sentencedDocuments map (kd => kd.copy(doc=fullExtractor.extract(kd.doc)))
  
  val psout = new java.io.PrintStream(args(1))
  val diffPrinter = new LinkDiffPrinter(psout)
  psout.println(diffPrinter.columnHeaderString)
  baseLineExtracted.zip(fullExtracted).map { case (base, full) =>
    diffPrinter.print(base.asInstanceOf[LinkedDocument], full.asInstanceOf[LinkedDocument])  
  }
  psout.flush()
  psout.close()
}

class LinkDiffPrinter(out: java.io.PrintStream) {

  import LinkDiffPrinter.LinkedDocument  
  
  def print(oldDoc: LinkedDocument, newDoc: LinkedDocument): Unit = {
    
    val oldLinks = oldDoc.links.toSet
    val newLinks = newDoc.links.toSet
    
    val oldDiff = oldLinks &~ newLinks
    val newDiff = newLinks &~ oldLinks
    
    // old links are paired with "true", new links "false".
    val combined = (oldDiff.map((true, _)) ++ newDiff.map((false, _)))
    // sort by offset
    val sorted = combined.toSeq.sortBy(_._2.offset)
    
    sorted foreach { case (oldFlag, link) =>
      out.println(diffString(oldFlag, link))
    }
  } 
  
  val columnHeaders = Seq("SOURCE", "OFFSET", "WIKI TITLE", "FB ID", "COMBINED SCORE", "DOC SIM SCORE", "INLINKS", "CROSSWIKIS SCORE")
  val columnHeaderString = columnHeaders.mkString("\t")
  
  def diffString(old: Boolean, link: FreeBaseLink): String = {
    val diffType = if (old) "BASELINE" else "NEW"
    diffType + "\t" + linkString(link)
  }
  
  def linkString(l: Link): String = {
    
    def fmt(d: Double) = "%.02f" format d
    
    l match {
        case f: FreeBaseLink =>
          val types = f.types.take(2).mkString(", ") + ", ..."
          val fields = Seq(f.offset, f.name, f.id, types, fmt(f.score), fmt(f.docSimScore), fmt(f.inlinks), fmt(f.candidateScore))
          fields.mkString("\t")
        case _ => s"(l.offset)\t${l.toString}"
    }
  }
}