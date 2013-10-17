package edu.knowitall.repr.link

import edu.knowitall.repr.tag.Tag
import edu.knowitall.repr.document.Document

trait Link extends Tag {
  def name: String
  def score: Double
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
}