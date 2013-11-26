package edu.knowitall.tool.bestmention
package classifier

import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.repr.bestmention.EntityType
import edu.knowitall.repr.bestmention.EntityType._
import edu.knowitall.repr.bestmention._
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.DocId
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.coref.CorefResolved

import BMFeature._
object BMFeature {
  type FeatureDoc = Document with CorefResolved with Sentenced[_ <: Sentence] with DocId
  
  def toDouble(f: RBMTuple => Boolean): RBMTuple => Double = {
    { rbmt => if (f(rbmt)) 1.0 else 0.0 }
  }
}

/** "Resolved Best-Mention tuple - 
 *  a resolved best mention and the document it came from.
 */
trait RBMTuple {
  type D <: FeatureDoc
  def bem: ResolvedBestMention
  def index: Int
  def doc: D
}
object RBMTuple {
  case class RBMTupleImpl(val bem: ResolvedBestMention, val index: Int, val doc: FeatureDoc) extends RBMTuple { type D = FeatureDoc }
  def apply[D <: FeatureDoc](bem: ResolvedBestMention, index: Int, doc: D): RBMTuple = RBMTupleImpl(bem, index, doc)
  def unapply(rbmt: RBMTuple): Option[(ResolvedBestMention, Int, FeatureDoc)] = Some((rbmt.bem, rbmt.index, rbmt.doc))
}

case class BMFeature(
  override val name: String,
  val func: RBMTuple => Double)
  extends Feature[RBMTuple, Double](name) {

  override def apply(that: RBMTuple): Double = this.func(that)
}

object BestMentionFeatures extends FeatureSet[RBMTuple, Double] {

  import BMFeature.toDouble
  import BestMentionHelper._
  import BMFeature._
  import edu.knowitall.tool.bestmention.BestMentionFinderOriginalAlgorithm.TipsterData

  def isTypeFeature(typ: EntityType): BMFeature = BMFeature(s"is a ${typ.name} rule", { case RBMTuple(bem, _, doc) =>
    if (bem.target.entityType == typ) 1.0 else 0.0
  })

  val typeFeatures = List(
    BMFeature("is CorefResolvedBestMention", toDouble(_.bem.isInstanceOf[CorefResolvedBestMention]))
  )

  val docFeatures = List(
    BMFeature("Ambiguous Candidate Count", _.bem.candidateCount)
  )

  val tipsterFeatures = List(
    BMFeature("Location Ambiguity Count", { case RBMTuple(bem, _, doc) =>
      if (bem.isInstanceOf[ContainerBestMention])
        TipsterData.totalCount(bem.target.cleanText.toLowerCase).toDouble
      else 0.0
    }),
    BMFeature("StateOrProvince contains City", toDouble { case RBMTuple(bem, _, doc) =>
      if (bem.isInstanceOf[ContainerBestMention]) {
        val cbm = bem.asInstanceOf[ContainerBestMention]
        stateContainsCity(cbm)
      } else false
    }),
    BMFeature("Country Contains City", toDouble { case RBMTuple(bem, _, doc) =>
      if (bem.isInstanceOf[ContainerBestMention]) {
        val cbm = bem.asInstanceOf[ContainerBestMention]
        countryContainsCity(cbm)
      } else false
    }),
    BMFeature("Target and Best are Both States", toDouble { case RBMTuple(bem, _, doc) =>
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
    val oneIsALocation = rbm.target.entityType == Location || rbm.containerEntity.entityType == Location
    oneIsALocation &&
    BestMentionFinderOriginalAlgorithm
    .sameLocationType(rbm.target.cleanText.toLowerCase, rbm.containerEntity.cleanText.toLowerCase)
  }

  def stateContainsCity(rbm: ContainerBestMention): Boolean = {
    TipsterData.cities.contains(rbm.target.cleanText.toLowerCase) &&
    TipsterData.stateOrProvinces.contains(rbm.containerEntity.cleanText.toLowerCase)
  }

  def countryContainsCity(rbm: ContainerBestMention): Boolean = {
    TipsterData.cities.contains(rbm.target.cleanText.toLowerCase) &&
    TipsterData.countries.contains(rbm.containerEntity.cleanText.toLowerCase)
  }

  def context(offset: Int, doc: FeatureDoc): String = {
    findSent(offset, doc)
    .map(_.sentence.text)
    .getOrElse {
        docContext(offset, doc)
    }
  }

  def targetContext(rbm: ResolvedBestMention, doc: FeatureDoc): String = context(rbm.target.offset, doc)

  def bestContext(rbm: ResolvedBestMention, doc: FeatureDoc): String = {
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

  private def findSent(offset: Int, doc: FeatureDoc) = {
    doc.sentences.find { ds =>
      val end = ds.offset + ds.sentence.text.length
      offset > ds.offset && offset < end
    }
  }

  private def docContext(offset: Int, doc: FeatureDoc) = {
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