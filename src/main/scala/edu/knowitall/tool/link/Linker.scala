package edu.knowitall.tool.link

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Postagged
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.tool.coref.CorefResolver
import edu.knowitall.repr.bestmention.BestMentionResolvedDocument
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.extraction.ExtractionPart
import edu.knowitall.tool.coref.Mention
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.tool.coref.Substitution
import edu.knowitall.tool.coref.CoreferenceResolver
import edu.knowitall.repr.link.LinkedDocument
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.browser.entity.EntityLinker
import scala.collection.JavaConverters._


trait OpenIELinked extends LinkedDocument {
  this: Document with Sentenced[Sentence with OpenIEExtracted] =>

  override type L = FreeBaseLink

  import OpenIELinked.ArgContext

  def links: Seq[L]
  /**
   * (arg to be linked, "cleaned" version of it's string, Seq[Context Sentence])
   */
  def argContexts: Seq[ArgContext]

}


object OpenIELinked {
  case class ArgContext(
    arg: ExtractionPart,
    cleanArg: String,
    source: DocumentSentence[Sentence with OpenIEExtracted],
    extended: Seq[DocumentSentence[Sentence with OpenIEExtracted]],
    clusters: Seq[MentionCluster]) {

    def fullText = (source +: extended).distinct.map(_.sentence.text)
    def size = extended.size + 1 // +1 for the required source field.
  }
}

trait OpenIELinker extends OpenIELinked {
  this: Document with Sentenced[Sentence with OpenIEExtracted] =>

  override type L = FreeBaseLink

  import OpenIELinked.ArgContext

  // Hardcoded threshold for linker score.
  val minCombinedScore = 5.0

  private def sentenceContaining(chStart: Int, chEnd: Int): Option[DocumentSentence[Sentence with OpenIEExtracted]] = {
    this.sentences.find { s =>
      val sStart = s.offset
      val sEnd   = s.offset + s.sentence.text.length
      chStart >= sStart && chEnd <= sEnd
    }
  }

  private def cleanArg(sent: DocumentSentence[Sentence with OpenIEExtracted])(arg: ExtractionPart): Seq[(Char, Int)] = {
    var tokens = arg.tokenIndices.map(index => sent.sentence.tokens(index))
    // drop determiners and prepositions from start/end
    if (tokens.headOption.exists(t => t.isDeterminer || t.isPreposition)) tokens = tokens.drop(1)
    if (tokens.lastOption.exists(t => t.isDeterminer || t.isPreposition)) tokens = tokens.dropRight(1)
    tokens.flatMap { t =>
      val baseOffset = t.offset + sent.offset
      t.string.zipWithIndex.map { case (ch, index) => (ch, index + baseOffset) } :+ (' ', baseOffset + t.string.length)
    }.dropRight(1)
  }

  /**
   * iterate over subs and remove any Substitutions, by their interval, that overlap with earlier subs.
   * e.g. ([0,1], [1,2], [2,3]) -> ([0,1], [2,3])
   */
  private def getNonOverlappingSubstitutions(subs: Seq[Substitution]): Seq[Substitution] = {
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


  private def resolveArg(indexedText: Seq[(Char, Int)], ds: DocumentSentence[Sentence with OpenIEExtracted]) = {
    val bestMentions = if (this.isInstanceOf[BestMentionResolvedDocument]) {
      (indexedText.headOption, indexedText.lastOption) match {
        case (Some((_, chStart)), Some((_, chEnd))) =>
          this.asInstanceOf[BestMentionResolvedDocument].bestMentionsBetween(indexedText.head._2, indexedText.last._2 + 1)
        case _ => Nil
      }

    } else Nil
    val subs = bestMentions.map(_.substitution)
    val filtered = getNonOverlappingSubstitutions(subs)
    if (filtered.isEmpty) indexedText.map(_._1).mkString
    else {
      val fixedSubs = filtered.map(_.fixPossessive)
      val subIntervals = fixedSubs.map { s => (Interval.open(s.mention.offset, s.mention.offset + s.mention.text.length), s.best.text) }
      val substituted = CoreferenceResolver.substitute(indexedText, subIntervals)
      substituted.map(_._1).mkString
    }
  }

  /**
   * Pairs of (argument, context)
   */
  lazy val argContexts: Seq[ArgContext] = this.sentences.flatMap { s =>
    // Get arguments to send to the linker
    val args = s.sentence.extractions.flatMap(e => e.arg1 :: e.arg2 :: Nil).distinct
    val cleanArgs = args map cleanArg(s)
    val resolvedArgs = cleanArgs.map(ca => resolveArg(ca, s))
    val foo = 1
    // Get context and cleaned-up form for each arg
    args.zip(resolvedArgs).map { case (arg, cleaned) =>
      val (extended, clusters) = if (this.isInstanceOf[CorefResolved]) {
        val argStartToken = s.sentence.tokens(arg.tokenIndices.head)
        val argEndToken = s.sentence.tokens(arg.tokenIndices.last)
        val chStart = argStartToken.offset + s.offset
        val chEnd = argEndToken.offset + argEndToken.string.length + s.offset
        val thisCoref = this.asInstanceOf[CorefResolved]
        def otherMentions = thisCoref.mentionsBetween(chStart, chEnd).map(_.asInstanceOf[Mention])
        val otherClusters = otherMentions.flatMap(thisCoref.cluster)
        val otherClusterMentions = otherClusters.flatMap(_.mentions)
        val extended = otherClusterMentions.flatMap { m =>
          sentenceContaining(m.offset, m.offset + m.text.length)
        }
        (extended, otherClusters)
      } else {
        (Nil, Nil)
      }
      ArgContext(arg, cleaned, s, extended, clusters)
    }
  }

  def linker: EntityLinker

  lazy val links: Seq[FreeBaseLink] = argContexts.flatMap { ac =>
    val elink = linker.getBestEntity(ac.cleanArg, ac.fullText).filter(_.combinedScore >= minCombinedScore)
    val linkedMention = elink.map { l =>
      val offset = ac.source.sentence.tokens(ac.arg.tokenIndices.head).offset + ac.source.offset
      FreeBaseLink(ac.arg.text, offset, l.entity.name, l.combinedScore, l.docSimScore, l.candidateScore, l.inlinks, l.entity.fbid, l.entity.retrieveTypes().asScala.toSeq)
    }
    linkedMention.toSeq
  }
}