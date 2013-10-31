package edu.knowitall.repr.coref

import edu.knowitall.tool.coref.Mention

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.tag.Tag

trait MentionCluster[M <: Mention] {
  def best: M
  def mentions: Seq[M]

}

trait CorefResolvedSuperTrait {
  type M <: Mention
  def clusters: Seq[MentionCluster[M]]
}

trait CorefResolved extends CorefResolvedSuperTrait {
  this: Document =>

  private lazy val clusterMap = clusters.flatMap(c => c.mentions.map(m => (m, c))).toMap

  private lazy val mentions = clusterMap.keys.toSeq

  def cluster(m: M) = clusterMap.get(m)

  /**
   * Get links contained between the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def mentionsBetween(chStart: Int, chEnd: Int): Seq[M] = {
    mentions.filter(m => m.offset >= chStart && (m.offset + m.text.length) <= chEnd)
  }

  /**
   * Get links overlapping the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def mentionsIntersecting(chStart: Int, chEnd: Int): Seq[M] = {
    mentions.filter(m => m.offset < chEnd && (m.offset + m.text.length) > chStart)
  }

  /**
   * Get links exactly matching the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def mentionsExact(chStart: Int, chEnd: Int): Seq[M] = {
    mentions.filter(m => m.offset == chStart && (m.offset + m.text.length) == chEnd)
  }
}