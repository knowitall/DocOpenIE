package edu.knowitall.tool.link

import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.knowitall.tool.coref.CorefResolver
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.link.FreeBaseLink
import edu.knowitall.repr.coref.Mention
import edu.knowitall.repr.link.LinkedDocument
import edu.knowitall.repr.document.Document
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

  /**
   * Pairs of (argument, context)
   */
  lazy val argContexts = this.sentences.flatMap { s =>
    val args = s.sentence.extractions.flatMap(e => e.arg1 +: e.arg2s)
    args.map(a => (a, s))
  }

  def linker: EntityLinker

  lazy val links: Seq[FreeBaseLink] = argContexts.flatMap { case (arg, context) =>
    val elink = linker.getBestEntity(arg.text, Seq(context.sentence.text))
    val linkedMention = elink.map { l =>
      FreeBaseLink(arg.text, arg.offset, l.entity.name, l.combinedScore, l.docSimScore, l.candidateScore, l.inlinks, l.entity.fbid, l.entity.retrieveTypes().asScala.toSeq)
    }
    linkedMention.toSeq
  }
}