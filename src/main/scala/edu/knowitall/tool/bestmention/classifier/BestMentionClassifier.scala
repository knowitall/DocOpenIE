package edu.knowitall.tool.bestmention.classifier

import edu.knowitall.tool.conf.WekaInstanceCollection
import edu.knowitall.tool.conf.WekaLogisticTrainer
import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.repr.bestmention.ResolvedBestMention
import weka.classifiers.AbstractClassifier

object BestMentionClassifier {
  
  var breeze = true
  
  val wekaBestMentionTrainer = new WekaLogisticTrainer(BestMentionFeatures) 
  val breezeBestMentionTrainer = new BreezeLogisticRegressionTrainer(BestMentionFeatures)
  
  def apply(training: Seq[Labelled[ResolvedBestMention]]) = {
    if (breeze)
      breezeBestMentionTrainer.train(training)
    else 
      wekaBestMentionTrainer.train(training)
  }
}