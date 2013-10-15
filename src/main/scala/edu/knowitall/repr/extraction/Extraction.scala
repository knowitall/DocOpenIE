package edu.knowitall.repr.extraction

import edu.knowitall.tool.typer.Type
import edu.knowitall.repr.coref.Mention
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.srlie.SrlExtraction

trait ExtractionPart {
  def text: String
  def tokenIndices: Seq[Int]
}

trait Extraction {
  def arg1: ExtractionPart
  def rel: ExtractionPart
  def arg2s: Seq[ExtractionPart]
  def confidence: Double
}

object Extraction {

  case class ExtractionPartImpl(val text: String, val tokenIndices: Seq[Int]) extends ExtractionPart

  case class ExtractionImpl(val arg1: ExtractionPart, val rel: ExtractionPart, val arg2s: Seq[ExtractionPart], val confidence: Double) extends Extraction

  def fromRelnoun(inst: BinaryExtractionInstance[ChunkedToken], confidence: Double): Extraction = {
    val rel = ExtractionPartImpl(inst.extr.rel.text, inst.extr.rel.tokenInterval)
    val arg1 = ExtractionPartImpl(inst.extr.arg1.text, inst.extr.arg1.tokenInterval)
    val arg2s = Seq(ExtractionPartImpl(inst.extr.arg2.text, inst.extr.arg2.tokenInterval))
    ExtractionImpl(arg1, rel, arg2s, confidence)
  }

  def fromSrlie(inst: SrlExtractionInstance, confidence: Double): Extraction = {
    def tokenIndices(part: SrlExtraction.MultiPart) = part.intervals.flatten
    val rel = ExtractionPartImpl(inst.extr.rel.text, inst.extr.rel.tokenIntervals.flatten)
    val arg1 = ExtractionPartImpl(inst.extr.arg1.text, inst.extr.arg1.tokenInterval)
    val arg2s = inst.extr.arg2s.map { arg2 => ExtractionPartImpl(arg2.text, arg2.tokenInterval) }
    ExtractionImpl(arg1, rel, arg2s, confidence)
  }
}