package edu.knowitall.tool.bestmention.classifier.eval

import java.io.File
import edu.knowitall.main.FullDocSerializer
import edu.knowitall.tool.bestmention.classifier._
import edu.knowitall.common.Resource.using

object BestMentionClassifierAnalysis {

  def main(args: Array[String]): Unit = {
    
    val trainingFile = new File(args(0))
    val docsPath = new File(args(1))
    val prOutFile = new File(args(2))
    val dataOutFile = new File(args(3))
    require(trainingFile.exists() && trainingFile.isFile(), "Invalid training file")
    require(docsPath.exists() && docsPath.isDirectory(), "Invalid doc path")
    val labelledReader = LabelledResolvedBestMentionReader(trainingFile, docsPath)
    val classifier = BestMentionClassifier(labelledReader)
    val confidences = labelledReader.map(_.item).map(classifier.apply)
    val precs = precRecall(labelledReader.map(_.label))
    val zipped = {
      labelledReader.labelledResolvedBestMentions
      .zip(confidences)
      .zip(precs)
      .map({case (((labelled, index, doc), conf), prec) => ((labelled, index, doc), conf, prec)})
      .toSeq
    }
    val sorted = zipped.sortBy({case((labelled, index, doc), conf, prec) => if (labelled.label) Double.MinValue else -conf})
    val dataOutLines = sorted.map { case((labelled, index, doc), conf, prec) =>
      Seq(
        "%.03f" format prec,               //precision
        "%.03f" format conf,               //confidence
        if (labelled.label) "1" else "0"   //label (1 or 0)
      ).mkString("\t") + ResolvedBestMentionWriter.writeRBM(index, labelled.item, doc)
    }
    using(new java.io.PrintWriter(prOutFile)) { prOut => 
      precs.map("%.03f".format(_))
      .map(prOut.println)
    }
    using(new java.io.PrintWriter(dataOutFile)) { dataOut => 
      dataOutLines.map(dataOut.println)
    }
  }

  def precRecall(sorted: Iterable[Boolean]): List[Double] = {
    

    var result: List[Double] = Nil

    var total = 0
    var correct = 0

    for (label <- sorted) {
      total += 1
      if (label) {
        correct += 1
      }
      result ::= (correct.toDouble / total.toDouble)
    }
    result.reverse//.tails.filter(_.nonEmpty).toSeq.map { tail => tail.max }
  }
}