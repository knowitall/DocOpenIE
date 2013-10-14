package edu.knowitall.repr.extraction

import edu.knowitall.tool.typer.Type
import edu.knowitall.repr.coref.Mention

trait Argument extends Mention {
  override val name = "Argument"
}

trait Relation extends Mention {
  override val name = "Relation"
}

trait Extraction {
  def arg1: Argument
  def rel: Relation
  def arg2s: Seq[Argument]
  def confidence: Double
}