package edu.knowitall.prep

case class KbpSentence(val docId: String, val sentNum: Int, val offset: Int, val text: String) {
  def length = text.length
}

object KbpSentence {

val tabRegex = "\t".r

  def read(pickle: String): Option[KbpSentence] = {
    tabRegex.split(pickle) match {
      case Array(docId, sentNum, offset, text, _*) => Some(KbpSentence(docId, sentNum.toInt, offset.toInt, text))
      case _ => {
        System.err.println("Error reading KbpSentence: %s".format(pickle))
        None
      }
    }
  }

  def write(sent: KbpSentence): String = {
    val fieldTuple = KbpSentence.unapply(sent).getOrElse {
      throw new RuntimeException("Unable to serialize KbpSentence")
    }
    val fields = fieldTuple.productIterator.map(_.toString).toSeq
    fields.map(_.replaceAll("\t", "")).mkString("\t")
  }
}