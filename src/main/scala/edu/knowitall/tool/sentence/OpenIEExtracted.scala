package edu.knowitall.tool.sentence

import edu.knowitall.repr.sentence.Extracted
import edu.knowitall.repr.sentence.Chunked
import edu.knowitall.repr.sentence.Parsed
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.repr.sentence.Lemmatized
import edu.knowitall.tool.srl.Srl
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.chunkedextractor.confidence.RelnounConfidenceFunction
import edu.knowitall.srlie.confidence.SrlConfidenceFunction
import edu.knowitall.repr.extraction.Extraction

trait OpenIEExtracted extends Extracted with Lemmatized with Chunked with Parsed {
  this: Sentence with Lemmatized with Chunked with Parsed  =>

  import OpenIEExtracted._

  lazy val srlExtrs = srlie(this.dgraph)
  lazy val relnounExtrs = relnoun(this.lemmatizedTokens)

  private def relnounConverted = relnounExtrs map { re => Extraction.fromRelnoun(re, relnounConf(re)) }
  private def srlieConverted = srlExtrs map { se => Extraction.fromSrlie(se, srlieConf(se)) }

  lazy val extractions = relnounConverted ++ srlieConverted
}

object OpenIEExtracted {

  val srl = new ClearSrl()
  val srlie = new SrlExtractor(srl)
  val relnoun = new Relnoun()

  val srlieConf = SrlConfidenceFunction.loadDefaultClassifier()
  val relnounConf = RelnounConfidenceFunction.loadDefaultClassifier()
}