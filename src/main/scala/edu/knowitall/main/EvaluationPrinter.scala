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
import edu.knowitall.tool.coref.Substitution
import edu.knowitall.tool.coref.CoreferenceResolver
import edu.knowitall.tool.bestentitymention.BestEntityMentionsFound
import edu.knowitall.repr.bestentitymention.BestEntityMention
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.bestentitymention.BestEntityMentionResolvedDocument
import edu.knowitall.repr.link.LinkedDocument

object EvaluationPrinter {
  private val cleaner = "\t|\n".r.pattern
  def clean(s: String): String = cleaner.matcher(s).replaceAll(" ")
}

/**
 * Code for producing tab-delimited output for human annotation and evaluation.
 */
class EvaluationPrinter(out: java.io.PrintStream) {

  import EvaluationPrinter._

  var extractionsPrintedCount = 0
  var extractionsLinked = 0
  var extractionsResolved = 0

  type SENT = Sentence with OpenIEExtracted
  type ExtractedSentenced = Sentenced[SENT]
  type CorefTraits = LinkedDocument with CorefResolved with ExtractedSentenced with BestEntityMentionResolvedDocument
  type BaselineTraits = OpenIELinked with ExtractedSentenced
  type FullTraits = OpenIELinked with ExtractedSentenced with BestEntityMentionsFound

  val columnHeaders = Seq("Best Arg1", "Rel", "Best Arg2", "Original Arg1", "Original Arg2", "Sentence Text", "Arg1 Links", "Arg2 Links", "Arg1 Best Mentions", "Arg2 Best Mentions", "Doc ID", "Arg1 Changed?", "Arg2 Changed?")

  val columnHeaderString = columnHeaders.mkString("\t")

  def printColumnHeaderString() = out.println(columnHeaderString)

  private def offset(epart: ExtractionPart, ds: DocumentSentence[SENT]) = ds.offset + ds.sentence.tokens(epart.tokenIndices.head).offset

  /**
   * iterate over subs and remove any Substitutions, by their interval, that overlap with earlier subs.
   * e.g. ([0,1], [1,2], [2,3]) -> ([0,1], [2,3])
   */
  def getNonOverlappingSubstitutions(subs: Seq[Substitution]): Seq[Substitution] = {
    var subsToKeep = List.empty[Substitution]
    var usedIntervals = List.empty[Interval]
    for (s <- subs) {
      val sInterval = Interval.open(s.mention.offset, s.mention.offset + s.mention.text.length)
      if (!usedIntervals.exists(_.intersects(sInterval))) {
        usedIntervals ::= sInterval
        subsToKeep ::= s
      }
    }
    subsToKeep
  }

  def getBestDisplayMention(epart: ExtractionPart, d: Document with ExtractedSentenced, ds: DocumentSentence[SENT]) = {
    val links = getLinks(epart, d, ds)
    val bestMentions = getBestEntityMentions(epart, d, ds)
    val subs = (links.map(_.substitution)++bestMentions.map(_.substitution))
    val filtered = getNonOverlappingSubstitutions(subs)
    if (filtered.isEmpty) epart.text
    else {
      val eOffset = offset(epart, ds)
      val indexedText = epart.text.zipWithIndex.map(p => (p._1, p._2+eOffset))
      val fixedSubs = subs.map(_.fixPossessive)
      val subIntervals = fixedSubs.map { s => (Interval.open(s.mention.offset, s.mention.offset + s.mention.text.length), s.best.text) }
      val substituted = CoreferenceResolver.substitute(indexedText, subIntervals)
      substituted.map(_._1).mkString
    }
  }

  def getLinks(epart: ExtractionPart, d: Document, ds: DocumentSentence[SENT]) = {
    if (d.isInstanceOf[LinkedDocument]) {
      d.asInstanceOf[LinkedDocument].linksBetween(offset(epart, ds), offset(epart, ds)+epart.text.length)
    } else Nil
  }

  def getBestEntityMentions(epart: ExtractionPart, d: Document, ds: DocumentSentence[SENT]) = {
    if (d.isInstanceOf[BestEntityMentionResolvedDocument]) {
      d.asInstanceOf[BestEntityMentionResolvedDocument].bestEntityMentionsBetween(offset(epart, ds), offset(epart, ds)+epart.text.length)
    } else Nil
  }

  def printFull(kd: KbpDocument[_ <: Document with ExtractedSentenced]): Unit = {

    val d = kd.doc

    for (docSent <- d.sentences) {

      for (e <- docSent.sentence.extractions) {

        val arg1Links = getLinks(e.arg1, d, docSent)
        val arg1BestMentions = getBestEntityMentions(e.arg1, d, docSent)
        val arg2Links = getLinks(e.arg2, d, docSent)
        val arg2BestMentions = getBestEntityMentions(e.arg2, d, docSent)
        val bestArg1Display = getBestDisplayMention(e.arg1, d, docSent)
        val bestArg2Display = getBestDisplayMention(e.arg2, d, docSent)
        val arg1Changed = bestArg1Display != e.arg1.text
        val arg2Changed = bestArg2Display != e.arg2.text
        if (arg1Changed || arg2Changed) {
          val arg1ChangedString = if (arg1Changed) "YES" else "NO"
          val arg2ChangedString = if (arg2Changed) "YES" else "NO"
          val arg1LinksString = arg1Links.map(_.debugString).mkString("[", ", ", "]")
          val arg2LinksString = arg2Links.map(_.debugString).mkString("[", ", ", "]")
          val arg1BestMentionsString = arg1BestMentions.map(_.debugString).mkString("[", ", ", "]")
          val arg2BestMentionsString = arg2BestMentions.map(_.debugString).mkString("[", ", ", "]")

          val fields = Seq(bestArg1Display, e.rel.text, bestArg2Display, e.arg1.text, e.arg2.text, docSent.sentence.text, arg1LinksString, arg2LinksString, arg1BestMentionsString, arg2BestMentionsString, kd.docId.trim, arg1ChangedString, arg2ChangedString)
          out.println(fields.map(clean).mkString("\t"))
          extractionsPrintedCount += 1
          if ((arg1Links ++ arg2Links).size > 0) extractionsLinked += 1
          if ((arg1BestMentions ++ arg2BestMentions).size > 0) extractionsResolved += 1
        }
      }
    }
  }
}