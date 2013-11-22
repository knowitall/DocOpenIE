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

object BestMentionFeatures extends FeatureSet[ResolvedBestMention, Double] {

  import BMFeature.toDouble
  import BestMentionHelper._

  def isTypeFeature(typ: EntityType): BMFeature = BMFeature(s"is a ${typ.name} rule", { bem: ResolvedBestMention =>
    if (bem.target.entityType == typ) 1.0 else 0.0
  })
  
  val typeFeatures = List(
    BMFeature("is ContainmentBestMention", toDouble(_.isInstanceOf[ContainmentBestMention])),
    BMFeature("is ContainerBestMention", toDouble(_.isInstanceOf[ContainerBestMention])),
    BMFeature("is CorefResolvedBestMention", toDouble(_.isInstanceOf[CorefResolvedBestMention])),
    BMFeature("is FullResolvedBestMention", toDouble(_.isInstanceOf[FullResolvedBestMention])),
    BMFeature("is LinkResolvedBestMention", toDouble(_.isInstanceOf[LinkResolvedBestMention])),
    BMFeature("Link Score", { bem => 
      if (bem.isInstanceOf[LinkResolvedBestMention])
        bem.asInstanceOf[LinkResolvedBestMention].link.score
      else 0.0
    })
  )
 
  val docFeatures = List(
    BMFeature("Ambiguous Candidate Count", _.candidateCount),
    BMFeature("Char Proximity", { bem => BestMentionHelper.charProximity(bem, 2500) }), // avg doc length is about 2500
    BMFeature("Target Precedes Best", toDouble { bem => 
      bem.isInstanceOf[FullResolvedBestMention] && bem.target.offset < bem.asInstanceOf[FullResolvedBestMention].bestEntity.offset
    }),
    BMFeature("Target After Best", toDouble { bem => 
      bem.isInstanceOf[FullResolvedBestMention] && bem.target.offset > bem.asInstanceOf[FullResolvedBestMention].bestEntity.offset
    })
  )
  
  val tipsterFeatures = List(
    BMFeature("StateOrProvince contains City", toDouble { bem =>
      if (bem.isInstanceOf[ContainerBestMention]) {
        val cbm = bem.asInstanceOf[ContainerBestMention]
        stateContainsCity(cbm)
      } else false
    }),
    BMFeature("Country Contains City", toDouble { bem =>
      if (bem.isInstanceOf[ContainerBestMention]) {
        val cbm = bem.asInstanceOf[ContainerBestMention]
        countryContainsCity(cbm)
      } else false
    }),
    BMFeature("Target and Best are Both States", toDouble { bem =>
      if (bem.isInstanceOf[ContainerBestMention]) {
        val cbm = bem.asInstanceOf[ContainerBestMention]
        bothStates(cbm)
      } else false
    })
  )
  
  val featuresList = EntityType.types.map(isTypeFeature) ++ typeFeatures ++ docFeatures ++ tipsterFeatures

  override val featureMap = scala.collection.immutable.SortedMap.empty[String, BMFeature] ++ featuresList.map(f => (f.name -> f)).toMap
}

object BestMentionHelper {
  
  import edu.knowitall.repr.document.DocId
  import edu.knowitall.repr.document.Document
  import edu.knowitall.repr.document.Sentenced
  import edu.knowitall.repr.sentence.Sentence
  
  import edu.knowitall.tool.bestmention.BestMentionFinderOriginalAlgorithm.locationContainsLocation
  import edu.knowitall.tool.bestmention.BestMentionFinderOriginalAlgorithm.TipsterData
  
  
  // a document that a resolved-best-mention might come from... hence R.B.M. Doc
  type RBMDoc = Document with Sentenced[_ <: Sentence] with BestMentionResolvedDocument with DocId
  
  def bothStates(rbm: ContainerBestMention): Boolean = {
    TipsterData.stateOrProvinces.contains(rbm.containerEntity.cleanText.toLowerCase) &&
    TipsterData.stateOrProvinces.contains(rbm.target.cleanText.toLowerCase)
  }
  
  def stateContainsCity(rbm: ContainerBestMention): Boolean = { 
    TipsterData.cities.contains(rbm.target.cleanText.toLowerCase) &&
    TipsterData.stateOrProvinces.contains(rbm.containerEntity.cleanText.toLowerCase)
  }
  
  def countryContainsCity(rbm: ContainerBestMention): Boolean = { 
    TipsterData.cities.contains(rbm.target.cleanText.toLowerCase) &&
    TipsterData.countries.contains(rbm.containerEntity.cleanText.toLowerCase)
  }
  
  def context(offset: Int, doc: RBMDoc): String = {
    findSent(offset, doc)
    .map(_.sentence.text)
    .getOrElse {
        docContext(offset, doc)
    }
  }
  
  def targetContext(rbm: ResolvedBestMention, doc: RBMDoc): String = context(rbm.target.offset, doc)
  
  def bestContext(rbm: ResolvedBestMention, doc: RBMDoc): String = {
    if (rbm.isInstanceOf[FullResolvedBestMention]) {
      context(rbm.asInstanceOf[FullResolvedBestMention].bestEntity.offset, doc)
    } else if (rbm.isInstanceOf[ContainerBestMention]) {
      context(rbm.asInstanceOf[ContainerBestMention].containerEntity.offset, doc)
    } else if (rbm.isInstanceOf[ContainmentBestMention]) {
      Seq(context(rbm.target.offset, doc), 
          context(rbm.asInstanceOf[ContainmentBestMention].containerEntity.offset, doc)).mkString(" ")
    } else {
      "NA"
    }
  }
  
  private def findSent(offset: Int, doc: RBMDoc) = {
    doc.sentences.find { ds => 
      val end = ds.offset + ds.sentence.text.length
      offset > ds.offset && offset < end
    }
  }
  
  private def docContext(offset: Int, doc: RBMDoc) = {
    doc.text.drop(offset - 40).take(80).replaceAll("\\s", " ")
  }
  
  def charProximity(rbm: ResolvedBestMention, default: Int): Int = {
    if (rbm.isInstanceOf[FullResolvedBestMention]) {
      val fbm = rbm.asInstanceOf[FullResolvedBestMention]
      Math.abs(fbm.target.offset - fbm.bestEntity.offset)
    } else {
      default
    }
  }  
}