package edu.knowitall.main

import java.io.File
import edu.knowitall.prep.DocSplitter
import edu.knowitall.prep.KbpRawDoc
import edu.knowitall.prep.KbpProcessedDoc
import edu.knowitall.prep.KbpDocLine
import edu.knowitall.prep.KbpWebDocProcessor
import edu.knowitall.prep.util.LineReader
import edu.knowitall.prep.util.Line
import edu.knowitall.prep.Sentencer
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.tool.document.OpenIEDocumentExtractor
import edu.knowitall.tool.document.OpenIEBaselineExtractor
import edu.knowitall.tool.document.OpenIENoCorefDocumentExtractor
import edu.knowitall.tool.document.OpenIECorefExpandedDocumentExtractor

case class KbpDocument[D <: Document](val doc: D, val docId: String)

object DocOpenIEMain {

  val kbpSentencer = Sentencer.defaultInstance

  val docExtractor = new OpenIEDocumentExtractor()

  def loadKbpDocs(path: String): Seq[(File, KbpRawDoc, KbpProcessedDoc)] = {

    val docPath = new File(path)

    val docFiles = docPath.listFiles().filter(_.getName().endsWith("sgm"))

    val rawDocs = docFiles flatMap loadKbpDoc

    rawDocs map { case (file, doc) => (file, doc, processDoc(file, doc)) }
  }

  def loadSentencedDocs(path: String) = {
    loadKbpDocs(path).toSeq.map { case (file, rawDoc, procDoc) =>
      val text = rawDoc.getString
      val kbpSentences = kbpSentencer.convertToSentences(procDoc)
      val doc = new Document(text) with Sentenced[Sentence] {
        override val sentences = kbpSentences.toStream.map { ks =>
          val sent = new Sentence(ks.text)
          DocumentSentence(sent, ks.offset)
        }
      }
      KbpDocument(doc, procDoc.extractDocId.get)
    }
  }

 /**
  * Usage: provide path to KBP sample documents.
  */
  def main(args: Array[String]): Unit = Timing.timeThen {

    val sentencedDocuments = loadSentencedDocs(args(0))

    val extractedDocuments = sentencedDocuments map (kd => kd.copy(doc=docExtractor.extract(kd.doc)))

    val outFile = new File(args(1))
    val psout = new java.io.PrintStream(outFile)
    val evalPrinter = new EvaluationPrinter(psout)
    evalPrinter.printColumnHeaderString()
    extractedDocuments.foreach { ed =>
      evalPrinter.printCoref(ed)
    }
    psout.flush()
    System.err.println("Total extractions: " + extractedDocuments.flatMap(_.doc.sentences.map(_.sentence.extractions)).size)
    System.err.println("(String-)Changed extractions: " + evalPrinter.extractionsPrintedCount)
    System.err.println("Linked extractions (arg1 or arg2): " + evalPrinter.extractionsLinked)
    System.err.println("Best-Mention resolved extractions, arg1 or arg2: " + evalPrinter.extractionsResolved)
    psout.close()
  } { timeNs => System.err.println("Processing time: %s".format(Timing.Seconds.format(timeNs))) }

  def processDoc(file: File, rawDoc: KbpRawDoc): KbpProcessedDoc = {
    KbpWebDocProcessor.process(rawDoc) match {
      case Some(procDoc) => procDoc
      case None => {
        throw new RuntimeException(file.getName())
      }
    }
  }

  def loadKbpDoc(docFile: File): Seq[(File, KbpRawDoc)] = {
    val docs = using(LineReader.fromFile(docFile, "UTF8")) { lineReader =>
      val docSplitter = new DocSplitter(lineReader)
      docSplitter.toList
    }

    if (docs.size != 1) System.err.println(s"Warning, loaded ${docs.size} docs from ${docFile.getName()}")
    docs.map((docFile, _))
  }
}

