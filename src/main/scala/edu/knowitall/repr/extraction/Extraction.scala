package edu.knowitall.repr.extraction

import edu.knowitall.repr.tag.Tag
import edu.knowitall.tool.coref.Mention
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.srlie.SrlExtraction

trait ExtractionPart extends Tag {
  def tokenIndices: Seq[Int]
}

trait Extraction {
  def arg1: ExtractionPart
  def rel: ExtractionPart
  def arg2: ExtractionPart
  def confidence: Double
}

object Extraction {

  case class ExtractionPartImpl(val text: String, val offset: Int, val tokenIndices: Seq[Int]) extends ExtractionPart

  case class ExtractionImpl(val arg1: ExtractionPart, val rel: ExtractionPart, val arg2: ExtractionPart, val confidence: Double) extends Extraction

  def fromRelnoun(inst: BinaryExtractionInstance[ChunkedToken], confidence: Double): Extraction = {
    val rel = ExtractionPartImpl(inst.extr.rel.text, inst.extr.rel.offsetInterval.start, inst.extr.rel.tokenInterval)
    val arg1 = ExtractionPartImpl(inst.extr.arg1.text, inst.extr.arg1.offsetInterval.start, inst.extr.arg1.tokenInterval)
    val arg2 = ExtractionPartImpl(inst.extr.arg2.text, inst.extr.arg2.offsetInterval.start, inst.extr.arg2.tokenInterval)
    ExtractionImpl(arg1, rel, arg2, confidence)
  }

  def fromSrlie(inst: SrlExtractionInstance, confidence: Double): Seq[Extraction] = {
    def tokenIndices(part: SrlExtraction.MultiPart) = part.intervals.flatten
    inst.triplize(true).filter(_.extr.arg2s.isEmpty).flatMap { inst =>
      val rel = ExtractionPartImpl(inst.extr.rel.text, inst.extr.rel.intervals.flatten.min, inst.extr.rel.tokenIntervals.flatten)
      val arg1 = ExtractionPartImpl(inst.extr.arg1.text, inst.extr.arg1.interval.start, inst.extr.arg1.tokenInterval)
      inst.extr.arg2s.map { arg2 =>
        ExtractionPartImpl(arg2.text, arg2.interval.start, arg2.tokenInterval).asInstanceOf[Extraction]
      }
    }
  }
}