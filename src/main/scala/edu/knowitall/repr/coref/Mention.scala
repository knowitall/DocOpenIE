package edu.knowitall.repr.coref

import edu.knowitall.tool.coref.Mention

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.tag.Tag

trait MentionCluster {
  def best: Mention
  def mentions: Seq[Mention]
}

trait CorefResolved {
  this: Document =>

  def clusters: Seq[MentionCluster]
    
  private lazy val clusterMap = clusters.flatMap(c => c.mentions.map(m => (m, c))).toMap

  private lazy val mentions = clusterMap.keys.toSeq
  
  def cluster(m: Mention) = clusterMap.get(m)

  /**
   * Get links contained between the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def mentionsBetween(chStart: Int, chEnd: Int): Seq[Mention] = {
    mentions.filter(m => m.offset >= chStart && (m.offset + m.text.length) <= chEnd)
  }

  /**
   * Get links overlapping the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def mentionsIntersecting(chStart: Int, chEnd: Int): Seq[Mention] = {
    mentions.filter(m => m.offset < chEnd && (m.offset + m.text.length) > chStart)
  }

  /**
   * Get links exactly matching the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def mentionsExact(chStart: Int, chEnd: Int): Seq[Mention] = {
    mentions.filter(m => m.offset == chStart && (m.offset + m.text.length) == chEnd)
  }
}