package edu.knowitall.prep

import java.util.regex.Pattern

/**
 * Represents the lines of text in a single
 * KBP corpus document -
 * e.g. from <DOC> to </DOC>
 */
class KbpRawDoc(val lines: List[KbpDocLine]) {

  def getString = lines.flatMap(_.line).mkString

}

/**
 * Represents a line of characters from the raw KBP source corpus,
 * with byte offsets counting from 0 at the start of the "<DOC>" tag.
 */
case class KbpDocLine(val line: String, val offset: Int) {
  lazy val isBlank = line.trim.isEmpty()
  def debugString = "(%04d) %s".format(offset, line)
  def length = line.length()
}

object KbpProcessedDoc {
  val docIdPattern = Pattern.compile("^<DOC\\s+id=.*", Pattern.CASE_INSENSITIVE)
  val trailingWs = Pattern.compile("\\s+$")
  val quotes = Pattern.compile("^\"([^\"]+)\"$")

  /*
   * Fabricate a KbpLine where any author mention is inside an extractp-able
   * sentence, where the sentence's offsets are set such that the author
   * mention falls in the correct location.
   * Returns none if an author couldn't be found within the line.
   */
  def extractAuthor(authorLine: KbpDocLine): Option[KbpDocLine] = {
    // Author format is always either:
    // <POSTER> "jht...@gmail.com" &lt;jht...@gmail.com&gt; </POSTER>   (web)
    // (nothing for news)
    // (nothing for forums, for now, since each doc lists many authors)
    val str = authorLine.line

    // Pull the author's name out of the line with start and end offsets.
    val mention: Option[Mention] =
      if (str.startsWith("<POSTER>")) {
        // drop the tag, plus a space.
        // then take until the next ampersand
        val (text, quoted) = {
          val c1 = str.drop(9).takeWhile(c => c != '&' && c != '<' && c != '\n')
          val c2 = trailingWs.matcher(c1).replaceAll("")
          val matcher = quotes.matcher(c2)
          val quoted = matcher.find()
          val c3 = if (quoted) c2.drop(1).dropRight(1) else c2
          (c3, quoted)
        }
        val start = if (quoted) 10 else 9
        val end = start + text.length
        Some(Mention(text, start, end, authorLine))
      } else {
        None
      }

    mention match {
      case Some(mention) => fabricateSentence(mention, "This post was written by ", ".")
      case None => None
    }
  }

  // Fabricate a sentence containing the author's name where
  // Byte offsets still properly point to the name
  private def fabricateSentence(m: Mention, prefix: String, suffix: String): Option[KbpDocLine] = {

    // compute start bytes for the fabricated sentence
    val fabStart = m.start - prefix.length + m.kbpLine.offset
    val fabEnd = m.end + suffix.length + m.kbpLine.offset
    // if fabricated offsets point to bytes outside the document (e.g. negative)
    // then we can't hand this to the caller, they'll hit an OOB exception.
    if (fabStart < 0 || fabEnd > m.kbpLine.offset + m.kbpLine.length) None
    else {
      val fabSentence = Seq(prefix, m.text, suffix).mkString
      Some(new KbpDocLine(fabSentence, fabStart))
    }
  }

  def extractDate(kbpLine: KbpDocLine): Option[KbpDocLine] = {
    // Date format is always:
    // <DATETIME> 2007-10-22T10:31:03 </DATETIME> (web)
    // On its own line with no tags, usually prefixed by a location
    // Indicated many times per forum document
    val str = kbpLine.line

    val mention = {
      if (str.startsWith("<DATETIME>")) {
        val text = str.drop(11).takeWhile(_ != '<')
        Mention(text, 11, 11 + text.length, kbpLine)
      } else {
        Mention(str, 0, str.length, kbpLine)
      }
    }

    fabricateSentence(mention, "This post was written on ", ".")
  }
}

class KbpProcessedDoc(
  val docIdLine: KbpDocLine,
  val authorLine: Option[KbpDocLine],
  val datetimeLine: Option[KbpDocLine],
  val textLines: List[KbpDocLine]) {

  import KbpProcessedDoc._

  def debugText = {
    val allFields = Seq(docIdLine) ++ authorLine ++ datetimeLine ++ textLines
    allFields.map(_.debugString).mkString
  }

  /*
   * Extract docId string, assuming kbpLine contains it.
   */
  def extractDocId: Option[String] = {
    // Format is either:
    // <DOCID>id here...</DOCID>		(web)
    // <DOC id="AFP_ENG_20090531.0001" type="story" >	(news)
    // <doc id="bolt-eng-DF-183-195681-7948494">	(forum)
    val str = docIdLine.line

    if (str.startsWith("<DOCID>")) {
      // drop the tag and go until the closing tag.
      Some(str.drop(8).takeWhile(_ != ' '))
    } else if (docIdPattern.matcher(str).find()) {
      // drop the <DOC ID=" part, and take until the closing quote.
      Some(str.drop(9).takeWhile(_ != '\"'))
    } else {
      // convertToSentences reports the error for us...
      None
    }
  }

  def extractAuthor: Option[KbpDocLine] = authorLine flatMap { (a => KbpProcessedDoc.extractAuthor(a)) }

  def extractDate: Option[KbpDocLine] = datetimeLine flatMap { (d => KbpProcessedDoc.extractDate(d))}
}

/*
 * Represents a specific subsection of a given KbpLine.
 *
 */
case class Mention(val text: String, val start: Int, val end: Int, val kbpLine: KbpDocLine)

