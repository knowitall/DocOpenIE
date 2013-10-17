package edu.knowitall.repr.sentence

import edu.knowitall.tool.typer.Type
import edu.knowitall.tool.typer.Typer

trait Tagged {
  tokenized: Tokenized =>

  def tags: Seq[Type]

  def tags(charStartOffset: Int, charEndOffset: Int): Seq[Type]
}

trait Tagger extends Tagged {
  tokenized: Tokenized =>

  def tagger: Typer[token]

  override val tags: Seq[Type] = tagger.apply(tokens)
}