package edu.knowitall.repr.bestmention

import edu.knowitall.tool.bestmention.BestMentionFinderOriginalAlgorithm

trait TargetResolved {
  this: BestMention =>
    
  def target: Entity
  override def text = target.cleanText
  override def offset = target.offset
}

trait BestResolved {
  this: BestMention =>
    
  def bestEntity: Entity
  def bestMention = bestEntity.name
}

case class IdentityBestMention(target: Entity) extends ResolvedBestMention {
  def bestMention = target.cleanText
}

trait ResolvedBestMention extends BestMention with TargetResolved

object ResolvedBestMention {
  case class ResolvedBestMentionImpl(target: Entity, bestMention: String) extends ResolvedBestMention
  def apply(target: Entity, bestMention: String) = ResolvedBestMentionImpl(target, bestMention)
}

trait FullResolvedBestMention extends ResolvedBestMention with BestResolved

object FullResolvedBestMention {
  case class FullResolvedBestMentionImpl(target: Entity, bestEntity: Entity) extends ResolvedBestMention with BestResolved
  def apply(target: Entity, bestEntity: Entity) = FullResolvedBestMentionImpl(target, bestEntity)
}

case class ContainmentBestMention(
    target: Entity, containedEntity:Entity, containerEntity: Entity) extends ResolvedBestMention {
  
  def bestMention = containedEntity.name + ", " + containerEntity.name
}

case class ContainerBestMention(
    target: Entity, containerEntity: Entity) extends ResolvedBestMention {
  
  def bestMention = BestMentionFinderOriginalAlgorithm.locationCasing(text + ", " + containerEntity.name)
}