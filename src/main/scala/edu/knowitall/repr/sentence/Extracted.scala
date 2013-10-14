package edu.knowitall.repr.sentence

import edu.knowitall.repr.extraction.Extraction

trait Extracted {
  this: Sentence =>

  def extractions: Seq[Extraction]
}