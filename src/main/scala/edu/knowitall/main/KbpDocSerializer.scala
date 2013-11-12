package edu.knowitall.main

import edu.knowitall.repr.document.KbpDocumentSentencer
import edu.knowitall.repr.document.DocumentParser
import java.io.File
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import edu.knowitall.common.Resource.using

object KbpDocSerializer extends App {

  val sentencedDocuments = KbpDocumentSentencer.loadSentencedDocs(args(0)).take(1)

  val outputDir = new File(args(1))
  
  if (!outputDir.exists()) outputDir.mkdir()
  
  val parsedDocuments = sentencedDocuments.map(DocumentParser.defaultInstance.parse)
  
  for (pd <- parsedDocuments) {
    val outFile = new File(outputDir, pd.docId + ".bin")
    using(new ObjectOutputStream(new FileOutputStream(outFile))) { oos =>
      oos.writeObject(pd)  
    }
  }
}