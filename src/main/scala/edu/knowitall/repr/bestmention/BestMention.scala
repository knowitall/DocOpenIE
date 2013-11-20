package edu.knowitall.repr.bestmention

import edu.knowitall.repr.tag.Tag
import edu.knowitall.repr.document.Document
import edu.knowitall.tool.coref.Mention
import edu.knowitall.tool.coref.Substitution

sealed abstract class EntityType(val name: String)

object EntityType {
  
  case object Person extends EntityType("PERSON")
  case object Organization extends EntityType("ORGANIZATION")
  case object Location extends EntityType("LOCATION")
  case object Other extends EntityType("OTHER")
  
  def types = List(Person, Organization, Location, Other)
  
  private def typeNameMap = types.map(t => (t.name -> t)).toMap
  
  def from(s: String) = typeNameMap.get(s).getOrElse(Other)
}

case class Entity(val text: String, val offset: Int, val name: String, val entityType: EntityType) extends Tag {
  lazy val cleanText = text.replaceAll("\\s+", " ")
  lazy val nameWords = name.split(" ")
  lazy val textWords = text.split(" ")
}

trait BestMention extends Tag {
  def bestMention: String

  def substitution = {
    val original = Mention(this.text, this.offset)
    val best = Mention(this.bestMention, this.offset)
    Substitution(original, best)
  }

  def debugString: String = {
    s"($offset) $text -> $bestMention"
  }
}

object BestMention{
  case class bestMentionImpl(text: String, offset: Int, bestMention:String) extends BestMention
  def apply (text: String, offset: Int, bestMention:String) = {
	  bestMentionImpl(text,offset,bestMention)
  }
}
trait BestMentionResolvedDocument {
  this: Document =>

  type B <: BestMention

  def bestMentions: Seq[B];

  /**
   * Get links contained between the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def bestMentionsBetween(chStart: Int, chEnd: Int): Seq[B] = {
    bestMentions.filter(l => l.offset >= chStart && (l.offset + l.text.length) <= chEnd)
  }

  /**
   * Get links overlapping the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def bestMentionsIntersecting(chStart: Int, chEnd: Int): Seq[B] = {
    bestMentions.filter(l => l.offset < chEnd && (l.offset + l.text.length) > chStart)
  }

  /**
   * Get links exactly matching the character interval
   * defined by chStart (inclusive) and chEnd (exclusive)
   */
  def bestMentionsExact(chStart: Int, chEnd: Int): Seq[B] = {
    bestMentions.filter(l => l.offset == chStart && (l.offset + l.text.length) == chEnd)
  }
}