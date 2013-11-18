package edu.knowitall.repr.bestentitymention

import edu.knowitall.tool.bestentitymention.BestEntityMentionFinderOriginalAlgorithm

case class ResolvedBestEntityMention(
    text: String, 
    offset: Int, bestEntity:Entity) extends BestEntityMention {
  
  def bestEntityMention = bestEntity.name
}

case class ContainmentBestEntityMention(
    text: String, 
    offset: Int, containedEntity:Entity, containerEntity: Entity) extends BestEntityMention {
  
  def bestEntityMention = containedEntity.name + ", " + containerEntity.name
}

case class ContainerBestEntityMention(
    text: String, 
    offset: Int, containerEntity: Entity) extends BestEntityMention {
  
  def bestEntityMention = BestEntityMentionFinderOriginalAlgorithm.locationCasing(text + ", " + containerEntity.name)
}