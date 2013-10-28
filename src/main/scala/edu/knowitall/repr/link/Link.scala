package edu.knowitall.repr.link

import edu.knowitall.repr.tag.Tag
import edu.knowitall.repr.document.Document
import edu.knowitall.tool.coref.Substitution
import edu.knowitall.tool.coref.Mention

trait Link extends Tag {
  def name: String
  def score: Double

  def substitution = {
    val original = Mention(this.text, this.offset)
    val best = Mention(this.name, this.offset)
    Substitution(original, best)
  }
}

trait FreeBaseLink extends Link {
  def id: String
  def types: Seq[String]
  def docSimScore: Double
  def candidateScore: Double
  def inlinks: Double
}

object FreeBaseLink {
  case class FreeBaseLinkImpl(text: String, offset: Int, name: String, score: Double, docSimScore: Double, candidateScore: Double, inlinks: Double, id: String, types: Seq[String]) extends FreeBaseLink
  def apply(text: String, offset: Int, name: String, score: Double, docSimScore: Double, candidateScore: Double, inlinks: Double, id: String, types: Seq[String]) = FreeBaseLinkImpl(text, offset, name, score, docSimScore, candidateScore, inlinks, id, types)
}

trait LinkedDocument[L <: Link] {
  this: Document =>

  def links: Seq[L]




  /**
   * Get links contained between the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def linksBetween(chStart: Int, chEnd: Int): Seq[L] = {
    links.filter(l => l.offset >= chStart && (l.offset + l.text.length) <= chEnd)
  }

  /**
   * Get links overlapping the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def linksIntersecting(chStart: Int, chEnd: Int): Seq[L] = {
    links.filter(l => l.offset < chEnd && (l.offset + l.text.length) > chStart)
  }

  /**
   * Get links exactly matching the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def linksExact(chStart: Int, chEnd: Int): Seq[L] = {
    links.filter(l => l.offset == chStart && (l.offset + l.text.length) == chEnd)
  }
}