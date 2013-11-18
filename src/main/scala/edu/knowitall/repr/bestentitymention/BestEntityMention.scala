package edu.knowitall.repr.bestentitymention

import edu.knowitall.repr.tag.Tag
import edu.knowitall.repr.document.Document
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.coref.Substitution

case class Entity(val text: String, val offset: Int, val name: String, val entityType: String) extends Tag {
  lazy val cleanText = text.replaceAll("\\s+", " ")
  lazy val nameWords = name.split(" ")
  lazy val textWords = text.split(" ")
}

trait BestEntityMention extends Tag {
  def bestEntityMention: String

  def substitution = {
    val original = Mention(this.text, this.offset)
    val best = Mention(this.bestEntityMention, this.offset)
    Substitution(original, best)
  }

  def debugString: String = {
    s"($offset) $text -> $bestEntityMention"
  }
}

object BestEntityMention{
  case class BestEntityMentionImpl(text: String, offset: Int, bestEntityMention:String) extends BestEntityMention
  def apply (text: String, offset: Int, bestEntityMention:String) = {
	  BestEntityMentionImpl(text,offset,bestEntityMention)
  }
}
trait BestEntityMentionResolvedDocument {
  this: Document =>

  type B <: BestEntityMention

  def bestEntityMentions: Seq[B];


  /**
   * Get links contained between the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def bestEntityMentionsBetween(chStart: Int, chEnd: Int): Seq[B] = {
    bestEntityMentions.filter(l => l.offset >= chStart && (l.offset + l.text.length) <= chEnd)
  }

  /**
   * Get links overlapping the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def bestEntityMentionsIntersecting(chStart: Int, chEnd: Int): Seq[B] = {
    bestEntityMentions.filter(l => l.offset < chEnd && (l.offset + l.text.length) > chStart)
  }

  /**
   * Get links exactly matching the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def bestEntityMentionsExact(chStart: Int, chEnd: Int): Seq[B] = {
    bestEntityMentions.filter(l => l.offset == chStart && (l.offset + l.text.length) == chEnd)
  }
}