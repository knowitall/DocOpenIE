package edu.knowitall.repr.coref

import edu.knowitall.tool.coref.Mention

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.tag.Tag

trait MentionCluster[M <: Mention] {
  def best: M
  def mentions: Seq[M]
}

trait CorefResolved[M <: Mention] {
  this: Document =>

  private lazy val clusterMap = clusters.flatMap(c => c.mentions.map(m => (m, c))).toMap

  def cluster(m: M) = clusterMap.get(m)

  def clustersAt(offset: Int): Seq[MentionCluster[M]] = clusterMap.filter { case (m, c) =>
    m.charInterval.contains(offset)
  }.values.toSeq

  def clusters: Seq[MentionCluster[M]]
}