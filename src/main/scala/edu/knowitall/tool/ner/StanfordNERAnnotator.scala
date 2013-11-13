package edu.knowitall.tool.ner

import java.util.Properties
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.pipeline.Annotation
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.ner.StanfordNERAnnotated

class StanfordNERAnnotator {
  lazy val NERAnnotator = {
    val nerProperties = new Properties();
    nerProperties.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma, ner");
    nerProperties.put("clean.allowflawedxml", "true");
    nerProperties.put("ner.useSUTime", "false");
    new StanfordCoreNLP(nerProperties);
  }

  def annotate(d: Document): Annotation = {
    val doc = new Annotation(d.text)
    NERAnnotator.annotate(doc)
    doc
  }
}