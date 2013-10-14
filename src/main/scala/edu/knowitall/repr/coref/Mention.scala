package edu.knowitall.repr.coref

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.document.Document

trait Mention {
  def text: String
  def offset: Int
}

trait Linked {
  this: Mention =>
  def links: Seq[Link]
}

trait MentionCluster[M <: Mention] {
  def mentions: Set[M]
}

trait CorefResolved[M <: Mention] {
  this: Document =>

  def clusters: Seq[MentionCluster[M]]
}