package edu.knowitall.main

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.ParsedDocument
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.DocId
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
import edu.knowitall.tool.bestmention.BestMentionsFound
import edu.knowitall.repr.bestmention.BestMention
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.bestmention.BestMentionResolvedDocument
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

  type SENT = Sentence with OpenIEExtracted
  type ExtractedSentenced = Sentenced[SENT]
  type CorefTraits = LinkedDocument with CorefResolved with ExtractedSentenced with BestMentionResolvedDocument
  type BaselineTraits = OpenIELinked with ExtractedSentenced
  type FullTraits = OpenIELinked with ExtractedSentenced with BestMentionsFound

  type ExtractedKbpDoc = Document with ExtractedSentenced with OpenIELinked with DocId

  val columnHeaders = Seq(
    "Best Arg1",
    "Rel",
    "Best Arg2",
    "Original Arg1",
    "Original Arg2",
    "Sentence Text",
    "Arg1 Best Mentions",
    "Arg2 Best Mentions",
    "Arg1 Links",
    "Arg2 Links",
    "Original Arg1 Links",
    "Original Arg2 Links",
    "Doc ID",
    "Arg1 Changed?",
    "Arg2 Changed?")

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
    val bestMentions = getbestMentions(epart, d, ds)
    val subs = (links.map(_.substitution) ++ bestMentions.map(_.substitution))
    val filtered = getNonOverlappingSubstitutions(subs)
    if (filtered.isEmpty) epart.text
    else {
      val eOffset = offset(epart, ds)
      val indexedText = epart.text.zipWithIndex.map(p => (p._1, p._2 + eOffset))
      val fixedSubs = filtered.map(_.fixPossessive)
      val subIntervals = fixedSubs.map { s => (Interval.open(s.mention.offset, s.mention.offset + s.mention.text.length), s.best.text) }
      val substituted = CoreferenceResolver.substitute(indexedText, subIntervals)
      substituted.map(_._1).mkString
    }
  }

  def getBestArgs(extr: Extraction, d: Document with ExtractedSentenced, ds: DocumentSentence[SENT]) = {
    val bestArg1 = getBestDisplayMention(extr.arg1, d, ds)
    val bestArg2 = getBestDisplayMention(extr.arg2, d, ds)
    (bestArg1, bestArg2)
  }

  def getLinks(epart: ExtractionPart, d: Document, ds: DocumentSentence[SENT]) = {
    if (d.isInstanceOf[LinkedDocument]) {
      d.asInstanceOf[LinkedDocument].linksBetween(offset(epart, ds), offset(epart, ds) + epart.text.length)
    } else Nil
  }

  def getbestMentions(epart: ExtractionPart, d: Document, ds: DocumentSentence[SENT]) = {
    if (d.isInstanceOf[BestMentionResolvedDocument]) {
      d.asInstanceOf[BestMentionResolvedDocument].bestMentionsBetween(offset(epart, ds), offset(epart, ds) + epart.text.length)
    } else Nil
  }

  def printFull(baseDoc: ExtractedKbpDoc, compDoc: ExtractedKbpDoc): Unit = {

    // we assume sentencing and extractions are the same....
    val sentencePairs = baseDoc.sentences.zip(compDoc.sentences)

    for ((baseSent, compSent) <- sentencePairs) {

      require(baseSent.sentence.text == compSent.sentence.text)

      val extrPairs = baseSent.sentence.extractions.zip(compSent.sentence.extractions)

      for ((baseExtr, compExtr) <- extrPairs) {

        val bestBaseArgs @ (bestBaseArg1, bestBaseArg2) = getBestArgs(baseExtr, baseDoc, baseSent)
        val bestCompArgs @ (bestCompArg1, bestCompArg2) = getBestArgs(compExtr, compDoc, compSent)

        val extrChanged = !bestBaseArgs.equals(bestCompArgs)

        if (extrChanged) {

          val baseArg1Links = getLinks(baseExtr.arg1, baseDoc, baseSent)
          val baseArg2Links = getLinks(baseExtr.arg2, baseDoc, baseSent)
          val baseArg1LinksString = baseArg1Links.map(_.debugString).mkString("[", ", ", "]")
          val baseArg2LinksString = baseArg2Links.map(_.debugString).mkString("[", ", ", "]")

          val compArg1Links = getLinks(compExtr.arg1, compDoc, compSent)
          val compArg2Links = getLinks(compExtr.arg2, compDoc, compSent)
          val compArg1LinksString = compArg1Links.map(_.debugString).mkString("[", ", ", "]")
          val compArg2LinksString = compArg2Links.map(_.debugString).mkString("[", ", ", "]")

          val arg1BestMentions = getbestMentions(compExtr.arg1, compDoc, compSent)
          val arg2BestMentions = getbestMentions(compExtr.arg2, compDoc, compSent)
          val arg1BestMentionsString = arg1BestMentions.map(_.debugString).mkString("[", ", ", "]")
          val arg2BestMentionsString = arg2BestMentions.map(_.debugString).mkString("[", ", ", "]")

          val arg1ChangedString = if (bestCompArg1 != bestBaseArg1) "YES" else "NO"
          val arg2ChangedString = if (bestCompArg2 != bestBaseArg2) "YES" else "NO"

          val fields = Seq(
            bestCompArg1,
            compExtr.rel.text,
            bestCompArg2,
            bestBaseArg1,
            bestBaseArg2,
            compSent.sentence.text,
            arg1BestMentionsString,
            arg2BestMentionsString,
            compArg1LinksString,
            compArg2LinksString,
            baseArg1LinksString,
            baseArg2LinksString,
            compDoc.docId.trim,
            arg1ChangedString,
            arg2ChangedString)

          out.println(fields.map(clean).mkString("\t"))
          extractionsPrintedCount += 1
        }
      }
    }
  }
}