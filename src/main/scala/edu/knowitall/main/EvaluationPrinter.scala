package edu.knowitall.main

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.extraction.Extraction
import edu.knowitall.repr.extraction.ExtractionPart
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.link.Link
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.tool.link.OpenIELinked
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.bestentitymention.BestEntityMentionsFound
import edu.knowitall.repr.bestentitymention.BestEntityMention

/**
 * Code for producing tab-delimited output for human annotation and evaluation.
 */
class EvaluationPrinter(out: java.io.PrintStream) {

  type SENT = Sentence with OpenIEExtracted
  type ExtractedSentenced = Sentenced[SENT]
  type FullTraits = OpenIELinked with ExtractedSentenced with BestEntityMentionsFound
  type BaselineTraits = OpenIELinked with ExtractedSentenced

  val columnHeaders = Seq("Best Arg1", "Rel", "Best Arg2", "Sentence Text", "Best Rule-Based Arg1 Mention", "Best Rule-Based Arg2 Mention", "Arg1 Link", "Arg2 Link", "Original Arg1", "Original Arg2", "KBP DOC ID")

  val columnHeaderString = columnHeaders.mkString("\t")

  def printColumnHeaderString() = out.println(columnHeaderString)

  private def offset(epart: ExtractionPart, ds: DocumentSentence[SENT]) = ds.offset + ds.sentence.tokens(epart.tokenIndices.head).offset

  private def linkString(l: FreeBaseLink) = Seq(l.name, l.id, l.score).mkString("(", ",", ")")

  def printFull(kd: KbpDocument[_ <: Document with FullTraits]): Unit = {

    val d = kd.doc

    def getLinks(epart: ExtractionPart, ds: DocumentSentence[SENT]) = d.linksExact(offset(epart, ds), offset(epart, ds)+epart.text.length)
    def getBestEntityMentions(epart: ExtractionPart, ds: DocumentSentence[SENT]) = d.bestEntityMentionsBetween(offset(epart, ds), offset(epart, ds)+epart.text.length)

    def getBestDisplayMention(epart: ExtractionPart, links: Seq[FreeBaseLink], bestMentions: Seq[BestEntityMention]) = {
      links.headOption.map(_.name).getOrElse(bestMentions.headOption.map(_.bestEntityMention).getOrElse(epart.text))
    }

    for (docSent <- d.sentences) {

      out.println("["+docSent.sentence.text+"]")

      for (e <- docSent.sentence.extractions;
           arg2 <- e.arg2s) {

        val arg1Links = getLinks(e.arg1, docSent)
        val arg1BestMentions = getBestEntityMentions(e.arg1, docSent)

        val arg2Links = getLinks(arg2, docSent)
        val arg2BestMentions = getBestEntityMentions(arg2, docSent)

        val bestArg1Display = getBestDisplayMention(e.arg1, arg1Links, arg1BestMentions)
        val bestArg2Display = getBestDisplayMention(arg2, arg2Links, arg2BestMentions)
        val bestArg1Link = arg1Links.headOption.map(linkString).getOrElse("X")
        val bestArg2Link = arg2Links.headOption.map(linkString).getOrElse("X")
        val bestArg1EntityMention = arg1BestMentions.headOption.map(_.bestEntityMention).getOrElse("X")
        val bestArg2EntityMention = arg2BestMentions.headOption.map(_.bestEntityMention).getOrElse("X")

        val fields = Seq(bestArg1Display, e.rel.text, bestArg2Display, docSent.sentence.text, bestArg1EntityMention, bestArg2EntityMention, bestArg1Link, bestArg2Link, e.arg1.text, arg2.text, kd.docId.trim)
        out.println(fields.mkString("\t"))
      }
      out.println()
    }
  }

  def printBaseline(kd: KbpDocument[Document with BaselineTraits]): Unit = {

    val d = kd.doc

    def getLinks(epart: ExtractionPart, ds: DocumentSentence[SENT]) = d.linksExact(offset(epart, ds), offset(epart, ds)+epart.text.length)

    def getBestDisplayMention(epart: ExtractionPart, links: Seq[FreeBaseLink]) = {
      (links).headOption.map(_.name).getOrElse(epart.text)
    }

    for (docSent <- d.sentences) {

      out.println("["+docSent.sentence.text+"]")

      for (e <- docSent.sentence.extractions;
           arg2 <- e.arg2s) {

        val arg1Links = getLinks(e.arg1, docSent)

        val arg2Links = getLinks(arg2, docSent)



        val bestArg1Display = getBestDisplayMention(e.arg1, arg1Links)
        val bestArg2Display = getBestDisplayMention(arg2, arg2Links)
        val bestArg1Link = arg1Links.headOption.map(linkString).getOrElse("X")
        val bestArg2Link = arg2Links.headOption.map(linkString).getOrElse("X")

        val fields = Seq(bestArg1Display, e.rel.text, bestArg2Display, docSent.sentence.text, "X", "X", bestArg1Link, bestArg2Link, e.arg1.text, arg2.text, kd.docId.trim)
        out.println(fields.mkString("\t"))
      }
      out.println()
    }
  }
}