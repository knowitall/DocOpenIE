package edu.knowitall.repr.ner

import edu.stanford.nlp.pipeline.Annotation
import edu.knowitall.repr.document.Document
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import edu.knowitall.common.Resource.using

trait StanfordNERAnnotated {
  this: Document =>

  def NERAnnotatedDoc: Annotation
}

object StanfordSerializableNERAnnotated {

  def annotationBytes(annotation: Annotation): Array[Byte] = {
    using(new ByteArrayOutputStream()) { bos =>
      using (new ObjectOutputStream(bos)) { oos =>
        oos.writeObject(annotation)
      }
      bos.toByteArray()
    }
  }
}

trait StanfordSerializableNERAnnotated extends StanfordNERAnnotated {

  this: Document =>

  val annotationBytes: Array[Byte]

  override def NERAnnotatedDoc = using(new ObjectInputStream(new ByteArrayInputStream(annotationBytes))) { ois =>
    ois.readObject().asInstanceOf[Annotation]
  }
}