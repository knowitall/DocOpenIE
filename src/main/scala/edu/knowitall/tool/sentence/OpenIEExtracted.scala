package edu.knowitall.tool.sentence

import edu.knowitall.repr.sentence.Extracted
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Parsed
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.repr.extraction.Extraction
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.tool.srl.Srl
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.chunkedextractor.confidence.RelnounConfidenceFunction
import edu.knowitall.srlie.confidence.SrlConfidenceFunction
import edu.knowitall.repr.extraction.Extraction

trait OpenIEExtracted extends Extracted with Lemmatized with Chunked with Parsed {
  this: Sentence with Lemmatized with Chunked with Parsed  =>

  def srlExtrs: Seq[SrlExtractionInstance]
  def relnounExtrs: Seq[BinaryExtractionInstance[Relnoun.Token]]

  def extractions: Seq[Extraction]
}

trait OpenIEExtractor extends OpenIEExtracted {
  this: Sentence with Lemmatized with Chunked with Parsed  =>

  import OpenIEExtractor._

  val srlExtrs = srlie(this.dgraph)
  val relnounExtrs = relnoun(this.lemmatizedTokens)

  private def relnounConverted = relnounExtrs map { re => Extraction.fromRelnoun(re, relnounConf(re)) }
  private def srlieConverted = srlExtrs flatMap { se => Extraction.fromSrlie(se, srlieConf(se)) }

  val extractions = relnounConverted ++ srlieConverted
}

object OpenIEExtractor {

  lazy val srl = new ClearSrl()
  lazy val srlie = new SrlExtractor(srl)
  lazy val relnoun = new Relnoun()

  lazy val srlieConf = SrlConfidenceFunction.loadDefaultClassifier()
  lazy val relnounConf = RelnounConfidenceFunction.loadDefaultClassifier()
}