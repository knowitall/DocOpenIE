package edu.knowitall.main

import edu.knowitall.repr.document.KbpDocumentSentencer
import edu.knowitall.repr.document.DocumentParser
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.document.DocId
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Parsed
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.ner.StanfordSerializableNERAnnotated
import edu.knowitall.repr.bestentitymention.BestEntityMentionResolvedDocument
import edu.knowitall.tool.document.OpenIECorefExpandedDocumentExtractor
import edu.knowitall.tool.link.OpenIELinked
import java.io.File
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.FileOutputStream
import java.io.FileInputStream
import edu.knowitall.common.Resource.using
import com.twitter.chill.MeatLocker

object KbpDocSerializer {

  def main(args: Array[String]): Unit = {

    val sentencedDocuments = KbpDocumentSentencer.loadSentencedDocs(args(0))

    val outputDir = new File(args(1))

    if (!outputDir.exists()) outputDir.mkdir()

    val parsedDocuments = sentencedDocuments.map(DocumentParser.defaultInstance.parse)

    for (pd <- parsedDocuments) {
      val outFile = new File(outputDir, pd.docId + ".bin")
      using(new ObjectOutputStream(new FileOutputStream(outFile))) { oos =>
        oos.writeObject(MeatLocker(pd))
      }
    }
  }

  def deserializeFromFile(file: File): Document with Sentenced[Sentence with OpenIEExtracted] with CorefResolved with StanfordSerializableNERAnnotated with DocId = {
    using(new ObjectInputStream(new FileInputStream(file))) { ois =>
      val meatLocker = ois.readObject().asInstanceOf[MeatLocker[Document with Sentenced[Sentence with OpenIEExtracted] with CorefResolved with StanfordSerializableNERAnnotated with DocId]]
      meatLocker.get
    }
  }
}

object FullDocSerializer {
  
  def main(args: Array[String]): Unit = {

    val parsedDocuments = new File(args(0)).listFiles.map(KbpDocSerializer.deserializeFromFile)

    val outputDir = new File(args(1))

    if (!outputDir.exists()) outputDir.mkdir()

    val docExtractor = new OpenIECorefExpandedDocumentExtractor()
    
    val extractedDocuments = parsedDocuments.map(docExtractor.extract)
    
    for (ed <- extractedDocuments) {
      val outFile = new File(outputDir, ed.docId + ".bin")
      using(new ObjectOutputStream(new FileOutputStream(outFile))) { oos =>
        oos.writeObject(MeatLocker(ed))
      }
    }
  }

  def deserializeFromFile(file: File): Document with Sentenced[Sentence with OpenIEExtracted] with CorefResolved with StanfordSerializableNERAnnotated with DocId = {
    using(new ObjectInputStream(new FileInputStream(file))) { ois =>
      val meatLocker = ois.readObject().asInstanceOf[MeatLocker[Document with OpenIELinked with CorefResolved with Sentenced[Sentence with OpenIEExtracted] with StanfordSerializableNERAnnotated with BestEntityMentionResolvedDocument with DocId]]
      meatLocker.get
    }
  }
}