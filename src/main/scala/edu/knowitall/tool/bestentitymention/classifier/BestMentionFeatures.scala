package edu.knowitall.tool.bestmention.classifier

import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.repr.bestmention.EntityType
import edu.knowitall.repr.bestmention.EntityType._
import edu.knowitall.repr.bestmention._

case class BMFeature(
  override val name: String,
  val func: ResolvedBestMention => Double)
  extends Feature[ResolvedBestMention, Double](name) {

  override def apply(that: ResolvedBestMention): Double = this.func(that)
}

object BMFeature {
  def toDouble(f: ResolvedBestMention => Boolean): ResolvedBestMention => Double = {
    { bem => if (f(bem)) 1.0 else 0.0 }
  }
}

class BestMentionFeatures extends FeatureSet[ResolvedBestMention, Double] {

  import BMFeature.toDouble

  def isTypeFeature(typ: EntityType): BMFeature = BMFeature(s"is a ${typ.name} rule", { bem: ResolvedBestMention =>
    if (bem.target.entityType == typ) 1.0 else 0.0
  })
  
  val typeFeatures = List(
    BMFeature("is ContainmentBestMention", toDouble({ bem: ResolvedBestMention =>
      bem.isInstanceOf[ContainmentBestMention]
    })),
    BMFeature("is ContainerBestMention", toDouble({ bem: ResolvedBestMention =>
      bem.isInstanceOf[ContainerBestMention]
    })))

  val featuresList = EntityType.types.map(isTypeFeature) ++ typeFeatures

  override val featureMap = scala.collection.immutable.SortedMap.empty[String, BMFeature] ++ featuresList.map(f => (f.name -> f)).toMap
}