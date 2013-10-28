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

/**
 * Code for producing tab-delimited output for human annotation and evaluation.
 */
class EvaluationPrinter(out: java.io.PrintStream) {

  var extractionsPrintedCount = 0

  type SENT = Sentence with OpenIEExtracted
  type ExtractedSentenced = Sentenced[SENT]
  type FullTraits = OpenIELinked with ExtractedSentenced with BestEntityMentionsFound
  type BaselineTraits = OpenIELinked with ExtractedSentenced

  val columnHeaders = Seq("Best Arg1", "Rel", "Best Arg2", "Original Arg1", "Original Arg2", "Sentence Text", "Arg1 Links", "Arg2 Links", "Arg1 Best Mentions", "Arg2 Best Mentions", "Doc ID", "Arg1 Changed?", "Arg2 Changed?")

  val columnHeaderString = columnHeaders.mkString("\t")

  def printColumnHeaderString() = out.println(columnHeaderString)

  private def offset(epart: ExtractionPart, ds: DocumentSentence[SENT]) = ds.offset + ds.sentence.tokens(epart.tokenIndices.head).offset

  private def linkString(l: FreeBaseLink) = Seq(l.name, l.id, l.score).mkString("(", ",", ")")

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

  def linkString(l: Link): String = {
    l match {
        case f: FreeBaseLink =>
          val types = f.types.take(2).mkString(", ") + ", ..."
          f"(${f.offset})\t${f.name}\t${f.id}\t$types\t${f.score}%.02f=f(${f.docSimScore}%.02f, ${f.inlinks}%.02f, ${f.candidateScore}%.02f)"
        case _ => s"(l.offset)\t${l.toString}"
    }
  }

  def getDisplayMention(epart: ExtractionPart, sourceSent: DocumentSentence[SENT], subs: Seq[Substitution]): String = {
    val eOffset = offset(epart, sourceSent)
    val indexedText = epart.text.zipWithIndex.map(p => (p._1, p._2+eOffset))
    val fixedSubs = subs.map(_.fixPossessive)
    val subIntervals = fixedSubs.map { s => (Interval.open(s.mention.offset, s.mention.offset + s.mention.text.length), s.best.text) }
    val substituted = CoreferenceResolver.substitute(indexedText, subIntervals)
    substituted.map(_._1).mkString
  }

  def printFull(kd: KbpDocument[_ <: Document with FullTraits]): Unit = {

    val d = kd.doc

    def getLinks(epart: ExtractionPart, ds: DocumentSentence[SENT]) = d.linksBetween(offset(epart, ds), offset(epart, ds)+epart.text.length)
    def getBestEntityMentions(epart: ExtractionPart, ds: DocumentSentence[SENT]) = d.bestEntityMentionsBetween(offset(epart, ds), offset(epart, ds)+epart.text.length)

    def getBestDisplayMention(epart: ExtractionPart, ds: DocumentSentence[SENT], links: Seq[FreeBaseLink], bestMentions: Seq[BestEntityMention]) = {
      val subs = (links.map(_.substitution)++bestMentions.map(_.substitution))
      val filtered = getNonOverlappingSubstitutions(subs)
      if (filtered.isEmpty) epart.text
      else getDisplayMention(epart, ds, filtered)
    }

    for (docSent <- d.sentences) {

      for (e <- docSent.sentence.extractions) {

        val arg1Links = getLinks(e.arg1, docSent)
        val arg1BestMentions = getBestEntityMentions(e.arg1, docSent)
        val arg2Links = getLinks(e.arg2, docSent)
        val arg2BestMentions = getBestEntityMentions(e.arg2, docSent)
        val bestArg1Display = getBestDisplayMention(e.arg1, docSent, arg1Links, arg1BestMentions)
        val bestArg2Display = getBestDisplayMention(e.arg2, docSent, arg2Links, arg2BestMentions)
        val arg1Changed = bestArg1Display != e.arg1.text
        val arg2Changed = bestArg2Display != e.arg2.text
        if (arg1Changed || arg2Changed) {
          val arg1ChangedString = if (arg1Changed) "YES" else "NO"
          val arg2ChangedString = if (arg2Changed) "YES" else "NO"
          val arg1LinksString = arg1Links.map(linkString).mkString("[", ", ", "]")
          val arg2LinksString = arg2Links.map(linkString).mkString("[", ", ", "]")
          val arg1BestMentionsString = arg1BestMentions.map(bm => s"(${bm.offset} ${bm.text})").mkString("[", ", ", "]")
          val arg2BestMentionsString = arg2BestMentions.map(bm => s"(${bm.offset} ${bm.text})").mkString("[", ", ", "]")

          val fields = Seq(bestArg1Display, e.rel.text, bestArg2Display, e.arg1.text, e.arg2.text, docSent.sentence.text, arg1LinksString, arg2LinksString, arg1BestMentionsString, arg2BestMentionsString, kd.docId.trim, arg1ChangedString, arg2ChangedString)
          out.println(fields.mkString("\t"))
          extractionsPrintedCount += 1
        }
      }
    }
  }
}