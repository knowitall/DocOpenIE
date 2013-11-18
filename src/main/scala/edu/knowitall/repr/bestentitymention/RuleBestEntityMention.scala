package edu.knowitall.repr.bestentitymention

sealed trait RuleBestEntityMention extends BestEntityMention

case class RuleOneBestMention(val text: String, val offset: Int, val bestEntityMention: String, val foobars: Int = 1) extends RuleBestEntityMention