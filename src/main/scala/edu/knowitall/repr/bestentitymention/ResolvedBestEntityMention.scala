package edu.knowitall.repr.bestentitymention

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