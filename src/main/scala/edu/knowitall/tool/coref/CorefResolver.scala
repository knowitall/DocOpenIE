package edu.knowitall.tool.coref

import edu.knowitall.repr.document.Document
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.repr.link.Link
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.tool.sentence.OpenIEExtracted
import java.util.Properties
import edu.stanford.nlp.pipeline.StanfordCoreNLP

trait CorefResolver {

  def resolve(doc: Document): Seq[MentionCluster]
}

case class StanfordMentionCluster(val best: Mention, val mentions: List[Mention]) extends MentionCluster

class StanfordCorefResolver() extends CorefResolver {

  val coref = new CleanXmlStanfordResolver()

  override def resolve(doc: Document): Seq[MentionCluster] = {
    val clusters = try {
      coref.clusters(doc.text)
    } catch {
      case e: Exception => {
        e.printStackTrace()
        Nil
      }
    }
    clusters.iterator.toSeq.map { case (bestMention, allMentions) =>
      StanfordMentionCluster(bestMention, allMentions)
    }
  }
}

class CleanXmlStanfordResolver extends StanfordCoreferenceResolver {
  override lazy val corenlp = {
    val props = new java.util.Properties();
    props.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma, ner, parse, dcoref");
    props.put("clean.allowflawedxml", "true");
    new StanfordCoreNLP(props);
  }
}