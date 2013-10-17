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
}

object FreeBaseLink {
  case class FreeBaseLinkImpl(text: String, offset: Int, name: String, score: Double, id: String, types: Seq[String]) extends FreeBaseLink
  def apply(text: String, offset: Int, name: String, score: Double, id: String, types: Seq[String]) = FreeBaseLinkImpl(text, offset, name, score, id, types)
}

trait LinkedDocument[L <: Link] {
  this: Document =>

  def links: Seq[L]
}