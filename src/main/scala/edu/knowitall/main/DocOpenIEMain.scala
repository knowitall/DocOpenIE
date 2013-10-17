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
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.tool.document.OpenIEDocumentExtractor

object DocOpenIEMain {

  val kbpSentencer = Sentencer.defaultInstance

  val docExtractor = new OpenIEDocumentExtractor()

/**
 * Usage: provide path to KBP sample documents.
 */
  def main(args: Array[String]): Unit = {

    val docPath = new File(args(0))

    val docFiles = docPath.listFiles().filter(_.getName().endsWith("sgm"))

    val rawDocs = docFiles flatMap loadKbpDoc

    val procDocs = rawDocs map { case (file, doc) => (file, doc, processDoc(file, doc)) }

    val sentencedDocuments = procDocs.map { case (file, rawDoc, procDoc) =>
      val text = rawDoc.getString
      val kbpSentences = kbpSentencer.convertToSentences(procDoc)
      new Document(text) with Sentenced[Sentence] {
        override val sentences = kbpSentences.toStream.map { ks =>
          val sent = new Sentence(ks.text)
          DocumentSentence(sent, ks.offset)
        }
      }
    }

    val extractedDocuments = sentencedDocuments map docExtractor.extract

    extractedDocuments.foreach { ed =>
      ed.links foreach println
    }
  }

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