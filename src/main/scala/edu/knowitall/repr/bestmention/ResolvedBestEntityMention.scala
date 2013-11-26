package edu.knowitall.repr.bestmention

import edu.knowitall.tool.coref.Mention
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.bestmention.EntityType._
import edu.knowitall.tool.bestmention.BestMentionFinderOriginalAlgorithm
import edu.knowitall.repr.link.FreeBaseLink

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

trait Coref {
  this: BestMention =>
    
  def cluster: MentionCluster
}

trait CandidateCount {
  this: BestMention =>
  def candidateCount: Double
} 


case class IdentityBestMention(target: Entity) extends ResolvedBestMention {
  def bestMention = target.cleanText
  def candidateCount = 0
}

trait ResolvedBestMention extends BestMention with TargetResolved with CandidateCount
object ResolvedBestMention {
  case class ResolvedBestMentionImpl(target: Entity, bestMention: String, candidateCount: Double) extends ResolvedBestMention
  def apply(target: Entity, bestMention: String, candidateCount: Double) = ResolvedBestMentionImpl(target, bestMention, candidateCount)
}

trait FullResolvedBestMention extends ResolvedBestMention with BestResolved
object FullResolvedBestMention {
  case class FullResolvedBestMentionImpl(target: Entity, bestEntity: Entity, candidateCount: Double) extends FullResolvedBestMention
  def apply(target: Entity, bestEntity: Entity, candidateCount: Double) = FullResolvedBestMentionImpl(target, bestEntity, candidateCount: Double)
}

trait CorefResolvedBestMention extends ResolvedBestMention with Coref
object CorefResolvedBestMention {
  case class CorefResolvedBestMentionImpl(target: Entity, bestMention: String, cluster: MentionCluster, candidateCount: Double) extends CorefResolvedBestMention
  def apply(target: Entity, bestMention: String, cluster: MentionCluster, candidateCount: Double) = 
    CorefResolvedBestMentionImpl(target, bestMention, cluster, candidateCount)
} 

trait CorefFullResolvedBestMention extends CorefResolvedBestMention with FullResolvedBestMention
object CorefFullResolvedBestMention {
  case class CorefFullResolvedBestMentionImpl(target: Entity, bestEntity: Entity, cluster: MentionCluster, candidateCount: Double) extends CorefFullResolvedBestMention
  def apply(target: Entity, bestEntity: Entity, cluster: MentionCluster, candidateCount: Double) = 
    CorefFullResolvedBestMentionImpl(target, bestEntity, cluster, candidateCount)
} 

case class LinkResolvedBestMention(m: Mention, link: FreeBaseLink, cluster: MentionCluster, candidateCount: Double) extends CorefFullResolvedBestMention {
  import LinkResolvedBestMention._
  val bestEntity = linkEntity(link)
  val target = Entity(m.text, m.offset, m.text, bestEntity.entityType)
}

object LinkResolvedBestMention {
  private val locRegex = "location|place|city|country|state|province".r
  private val orgRegex = "business|corporation|company".r
  private val perRegex = "people|person".r
  
  def linkEntity(link: FreeBaseLink) = Entity(link.text, link.offset, link.name, guessType(link))
  
  def guessType(fb: FreeBaseLink): EntityType = {
    if (fb.types.exists(t => locRegex.findFirstIn(t).isDefined)) Location
    else if (fb.types.exists(t => orgRegex.findFirstIn(t).isDefined)) Organization
    else if (fb.types.exists(t => perRegex.findFirstIn(t).isDefined)) Person
    else Other
  }
}

trait ContainerBestMention extends ResolvedBestMention {
  def containerEntity: Entity
  def bestMention = BestMentionFinderOriginalAlgorithm.locationCasing(text + ", " + containerEntity.name)
}
object ContainerBestMention {
  case class ContainerBestMentionImpl(target: Entity, containerEntity: Entity, candidateCount: Double) extends ContainerBestMention
  def apply(target: Entity, containerEntity: Entity, candidateCount: Double) =
    ContainerBestMentionImpl(target, containerEntity, candidateCount)
}

case class ContainmentBestMention(
    target: Entity, containedEntity:Entity, containerEntity: Entity, candidateCount: Double) extends ContainerBestMention {
  
  override def bestMention = containedEntity.name + ", " + containerEntity.name
}

