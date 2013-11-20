package edu.knowitall.tool.bestmention.classifier

import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.repr.bestmention.EntityType.Person
import edu.knowitall.repr.bestmention.BestMention
import edu.knowitall.repr.bestmention.ResolvedBestMention

case class BMFeature(
    override val name: String, 
    val func: ResolvedBestMention => Double) 
extends Feature[ResolvedBestMention, Double](name) 
{
  override def apply(that: ResolvedBestMention): Double = this.func(that)
}

class BestMentionFeatures extends FeatureSet[ResolvedBestMention, Double] {
  val featuresList = List(
    BMFeature("is PERSON rule", { bem => 
      bem.target.entityType match {
        case Person => 1.0
        case _ => 0.0
      }
    }) 
    ,
    BMFeature("is ORGANIZATION rule", { bem => 
      bem.target.entityType match {
        case Person => 1.0
        case _ => 0.0
      }
    })
  ) // featuresList
  
  override val featureMap = scala.collection.immutable.SortedMap.empty[String, BMFeature] ++ featuresList.map(f => (f.name -> f)).toMap
}