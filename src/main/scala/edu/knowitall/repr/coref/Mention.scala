package edu.knowitall.repr.coref

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.tag.Tag

trait Mention extends Tag

trait MentionCluster[M <: Mention] {
  def mentions: Set[M]
}

trait CorefResolved[M <: Mention] {
  this: Document =>

  def clusters: Seq[MentionCluster[M]]
}