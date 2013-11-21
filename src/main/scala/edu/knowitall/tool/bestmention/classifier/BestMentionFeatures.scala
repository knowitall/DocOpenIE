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

  def isTypeFeature(typ: EntityType): BMFeature = BMFeature(s"is a ${typ.name} rule", { bem: ResolvedBestMention =>
    if (bem.target.entityType == typ) 1.0 else 0.0
  })
  
  val typeFeatures = List(
    BMFeature("is ContainmentBestMention", toDouble(_.isInstanceOf[ContainmentBestMention])),
    BMFeature("is ContainerBestMention", toDouble(_.isInstanceOf[ContainerBestMention])),
    BMFeature("is CorefResolvedBestMention", toDouble(_.isInstanceOf[CorefResolvedBestMention])),
    BMFeature("is FullResolvedBestMention", toDouble(_.isInstanceOf[FullResolvedBestMention])),
    BMFeature("is LinkResolvedBestMention", toDouble(_.isInstanceOf[LinkResolvedBestMention]))
  )
 
  val docFeatures = List(
    BMFeature("Ambiguous Candidate Count", _.candidateCount),
    BMFeature("Char Proximity", { bem => BestMentionHelper.charProximity(bem, 2500) }) // avg doc length is about 2500
  )
  
  val featuresList = EntityType.types.map(isTypeFeature) ++ typeFeatures ++ docFeatures

  override val featureMap = scala.collection.immutable.SortedMap.empty[String, BMFeature] ++ featuresList.map(f => (f.name -> f)).toMap
}

object BestMentionHelper {
  
  import edu.knowitall.repr.document.DocId
  import edu.knowitall.repr.document.Document
  import edu.knowitall.repr.document.Sentenced
  import edu.knowitall.repr.sentence.Sentence
  
  // a document that a resolved-best-mention might come from... hence R.B.M. Doc
  type RBMDoc = Document with Sentenced[_ <: Sentence] with BestMentionResolvedDocument with DocId
  
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