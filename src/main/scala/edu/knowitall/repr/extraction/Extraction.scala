package edu.knowitall.repr.extraction

import edu.knowitall.tool.typer.Type
import edu.knowitall.repr.coref.Mention
import edu.knowitall.collection.immutable.Interval

trait Argument extends Mention {
  def tokenInterval: Interval
}

trait Relation extends Mention {
  def tokenInterval: Interval
}

trait Extraction {
  def arg1: Argument
  def rel: Relation
  def arg2s: Seq[Argument]
  def confidence: Double
}