package edu.knowitall.repr.bestentitymention

import edu.knowitall.repr.tag.Tag
import edu.knowitall.repr.document.Document

trait BestEntityMention extends Tag {
  def bestEntityMention: String
}

object BestEntityMention{
  case class BestEntityMentionImpl(text: String, offset: Int, bestEntityMention:String ) extends BestEntityMention
  def apply (text: String, offset: Int, bestEntityMention:String) = {
	  BestEntityMentionImpl(text,offset,bestEntityMention)
  }
}
trait BestEntityMentionResolvedDocument[B <: BestEntityMention]{
  this: Document =>
    
  def bestEntityMentions: Seq[B];
}