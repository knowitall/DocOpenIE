package edu.knowitall.tool.bestmention.classifier

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocId
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.bestmention._
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.main.FullDocSerializer
import edu.knowitall.common.Resource.using
import BestMentionHelper.{targetContext, bestContext}
import java.io.File

object ResolvedBestMentionWriter {

  def main(args: Array[String]): Unit = {
    
    val docsDir = new File(args(0))
    val outfile = new File(args(1))
    require(docsDir.exists(), "doc path does not exist")
    require(docsDir.isDirectory(), "doc path is not a directory")
  
    val docs = docsDir.listFiles.toSeq.map { f => FullDocSerializer.deserializeFromFile(f) }
    
    using(new java.io.PrintStream(outfile)) { out =>
      out.println(featureHeaders.mkString("\t"))
      for (doc <- docs;
           line <- writeAll(doc)) {
        out.println(line)
      }            
    }
  }
  
  type RBMDoc = Document with Sentenced[_ <: Sentence] with BestMentionResolvedDocument with CorefResolved with DocId
  
  def writeAll(doc: RBMDoc): Seq[String] = {
    doc.bestMentions.zipWithIndex.map { case (rbm, index) =>
      writeRBM(RBMTuple(rbm, index, doc))
    }
  }
  
  val featureHeaders = Seq(
    "label",
    "target text",
    "best text",
    "target context",
    "best context") ++ BestMentionFeatures.featureNames() ++ Seq(
      "doc bem index",    
      "doc id"
    )
  
  def writeRBM(rbmt: RBMTuple): String = {
    
    def noTabs(s: String) = s.replaceAll("\t", " ").replaceAll("\n", " ")
    
    def twoPlaces(d: Double) = "%.02f" format d
    
    val featureVector = BestMentionFeatures.vectorize(rbmt).map(twoPlaces)
    
    val rbm = rbmt.bem
    val doc = rbmt.doc
    
    val fields = Seq(
      "",
      rbm.text,
      rbm.bestMention,
      targetContext(rbm, doc),
      bestContext(rbm, doc)) ++ featureVector ++ Seq(
      rbmt.index.toString,
      doc.docId
    )
    fields.map(noTabs).mkString("\t")
  }
}