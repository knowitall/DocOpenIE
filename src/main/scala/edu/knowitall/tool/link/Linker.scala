package edu.knowitall.tool.link

import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Postagged
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.tool.coref.CorefResolver
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.extraction.ExtractionPart
import edu.knowitall.tool.coref.Mention
import edu.knowitall.repr.link.LinkedDocument
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocumentSentence
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.browser.entity.EntityLinker
import scala.collection.JavaConverters._

trait Linker {

  type link <: Link

  type document <: Document

  def link(doc: document): Seq[link]
}


trait OpenIELinked extends LinkedDocument[FreeBaseLink] {
  this: Document with Sentenced[Sentence with OpenIEExtracted] =>

  val minCombinedScore = 4.5

  case class Context(source: DocumentSentence[Sentence with OpenIEExtracted], extended: Seq[DocumentSentence[Sentence with OpenIEExtracted]]) {
    def fullText = (source +: extended).distinct.map(_.sentence.text)
  }

  private def sentenceAt(offset: Int): Option[DocumentSentence[Sentence with OpenIEExtracted]] = {
    this.sentences.find { s =>
      val charStart = s.offset
      val charEnd   = s.offset + s.sentence.text.length
      offset >= charStart && offset <= charEnd
    }
  }

  private def cleanArg(sent: Sentence with Chunked)(arg: ExtractionPart): String = {
    var tokens = arg.tokenIndices.map(index => sent.tokens(index))
    // drop determiners and prepositions from start/end
    if (tokens.headOption.exists(t => t.isDeterminer || t.isPreposition)) tokens = tokens.drop(1)
    if (tokens.lastOption.exists(t => t.isDeterminer || t.isPreposition)) tokens = tokens.dropRight(1)
    tokens.map(_.string).mkString(" ")
  }

  /**
   * Pairs of (argument, context)
   */
  lazy val argContexts = this.sentences.flatMap { s =>
    val args = s.sentence.extractions.flatMap(e => e.arg1 :: e.arg2 :: Nil)
    val cleanArgs = args map cleanArg(s.sentence)
    args.zip(cleanArgs).map { case (arg, cleaned) =>
      val ao = s.sentence.tokens(arg.tokenIndices.head).offset + s.offset
      val extended = if (this.isInstanceOf[CorefResolved[_ <: Mention]]) {
        val otherMentions = this.asInstanceOf[CorefResolved[_ <: Mention]].clustersAt(ao).flatMap(_.mentions)
        otherMentions.map(_.offset).flatMap(sentenceAt).toList
      } else Nil
      val context = Context(s, extended)
      (arg, cleaned, context)
    }
  }

  def linker: EntityLinker

  lazy val links: Seq[FreeBaseLink] = argContexts.flatMap { case (arg, cleaned, context) =>
    val elink = linker.getBestEntity(cleaned, context.fullText).filter(_.combinedScore >= minCombinedScore)
    val linkedMention = elink.map { l =>
      val offset = context.source.sentence.tokens(arg.tokenIndices.head).offset + context.source.offset
      FreeBaseLink(arg.text, offset, l.entity.name, l.combinedScore, l.docSimScore, l.candidateScore, l.inlinks, l.entity.fbid, l.entity.retrieveTypes().asScala.toSeq)
    }
    linkedMention.toSeq
  }
}