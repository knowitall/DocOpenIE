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
import edu.knowitall.repr.document.KbpDocumentSentencer
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.repr.link.LinkedDocument
import edu.knowitall.tool.document.OpenIEDocumentExtractor
import edu.knowitall.tool.document.OpenIEBaselineExtractor
import edu.knowitall.tool.document.OpenIENoCorefDocumentExtractor
import edu.knowitall.tool.document.OpenIECorefExpandedDocumentExtractor
import edu.knowitall.repr.document.DocumentParser

object DocOpenIEMain {

 /**
  * Usage: provide path to KBP sample documents.
  */
  def main(args: Array[String]): Unit = Timing.timeThen {

    val sentencedDocuments = KbpDocumentSentencer.loadSentencedDocs(args(0))
    val parsedDocuments = sentencedDocuments.map(DocumentParser.defaultInstance.parse)
    
    val baselineSystem = new OpenIEBaselineExtractor()
    val comparisonSystem = new OpenIENoCorefDocumentExtractor()

    val baselineDocuments = parsedDocuments.map(baselineSystem.extract)
    val comparisonDocuments = parsedDocuments.map(comparisonSystem.extract)
    val docPairs = baselineDocuments.zip(comparisonDocuments)


    val outFile = new File(args(1))
    val psout = new java.io.PrintStream(outFile)
    val evalPrinter = new EvaluationPrinter(psout)
    evalPrinter.printColumnHeaderString()
    docPairs.foreach { case (baseline, comparison) =>
      evalPrinter.printFull(baseline, comparison)
    }
    psout.flush()
    System.err.println("Total extractions: " + comparisonDocuments.flatMap(_.sentences.map(_.sentence.extractions)).size)
    System.err.println("(String-)Changed extractions: " + evalPrinter.extractionsPrintedCount)
    System.err.println("Baseline Linked extractions (arg1 or arg2): " + baselineDocuments.flatMap(_.links).size)

    val comparisonLinkCounts = comparisonDocuments.map(_.links.size)

    System.err.println("Comparison Linked extractions (arg1 or arg2): " + comparisonLinkCounts.sum)

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