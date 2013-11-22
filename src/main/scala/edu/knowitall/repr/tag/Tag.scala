package edu.knowitall.repr.tag

trait Tag {
  def text: String
  def offset: Int
}

object Tag {
  case class TagImpl(val text: String, val offset: Int) extends Tag
  def apply(text: String, offset: Int): Tag = TagImpl(text, offset)
}