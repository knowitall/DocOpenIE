package edu.knowitall.tool.bestmention.classifier

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocId
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.bestmention._
import edu.knowitall.tool.bestmention.classifier.BestMentionFeatures
import edu.knowitall.main.FullDocSerializer
import edu.knowitall.common.Resource.using
import java.io.File

object ResolvedBestMentionWriter {

  def main(args: Array[String]): Unit = {
    
    val docsDir = new File(args(0))
    val outfile = new File(args(1))
    require(docsDir.exists(), "doc path does not exist")
    require(docsDir.isDirectory(), "doc path is not a directory")
  
    val docs = docsDir.listFiles.toSeq.map { f => FullDocSerializer.deserializeFromFile(f) }
    
    using(new java.io.PrintStream(outfile)) { out =>
      for (doc <- docs;
           line <- writeAll(doc)) {
        out.println(line)
      }            
    }
  }
  
  type RBMDoc = Document with Sentenced[_ <: Sentence] with BestMentionResolvedDocument with DocId
  
  def writeAll(doc: RBMDoc): Seq[String] = {
    doc.bestMentions.zipWithIndex.map { case (rbm, index) =>
      writeRBM(index, rbm, doc)
    }
  }
  
  private def writeRBM(index: Int, rbm: ResolvedBestMention, doc: RBMDoc): String = {
    
    def noTabs(s: String) = s.replaceAll("\t", " ")
    
    def twoPlaces(d: Double) = "%.02f" format d
    
    val featureVector = BestMentionFeatures.vectorize(rbm).map(twoPlaces)
    
    val fields = Seq(
      "",
      rbm.text,
      rbm.bestMention,
      context(rbm, doc),
      getBestContext(rbm, doc)) ++ featureVector ++ Seq(
      index.toString,
      doc.docId
    )
    fields.map(noTabs).mkString("\t")
  }
  
  private def findSent(offset: Int, doc: RBMDoc) = {
    doc.sentences.find { ds => 
      val end = ds.offset + ds.sentence.text.length
      offset > ds.offset && offset < end
    }
  }
  
  private def docContext(offset: Int, doc: RBMDoc) = {
    doc.text.drop(offset - 40).take(40).replaceAll("\\s", " ")
  }
  
  private def getBestContext(rbm: ResolvedBestMention, doc: RBMDoc): String = {
    if (rbm.isInstanceOf[FullResolvedBestMention]) {
      val offset = rbm.asInstanceOf[FullResolvedBestMention].bestEntity.offset
      val sentContext = findSent(offset, doc).map(_.sentence.text)
      sentContext.getOrElse(docContext(offset, doc))
    } else "No Best Context."
  }
  
  private def context(rbm: ResolvedBestMention, doc: RBMDoc): String = {
    findSent(rbm.offset, doc).map(_.sentence.text).getOrElse(docContext(rbm.offset, doc))
  }
}