package edu.knowitall.prep

import util.Asciifier

object SentenceFilter {

  val wsRegex = "\\s+".r

  /**
   * Perform some boolean checks against the sentence text-
   * Return None if any of them return false.
   * Otherwise, perform cleanup that _does not_ change the
   * character length of the sentence, and return
   * Some(cleanedSentence)
   */
  def apply(sentence: KbpSentence): Option[KbpSentence] = {

    val text = sentence.text

    // throw the sentence away if there aren't many "words" -
    // it should have a few whitespace gaps.
    if (sentence.text.length > 750) None
    else if (wsRegex.findAllIn(text).length < 3) None
    else Some(clean(sentence))
  }

  def clean(sentence: KbpSentence): KbpSentence = {

    val cleaned = Asciifier.apply(sentence.text)

    require(cleaned.length.equals(sentence.text.length))

    new KbpSentence(
        docId = sentence.docId,
        offset = sentence.offset,
        sentNum = sentence.sentNum,
        text = cleaned)
  }
}