package edu.knowitall.tool.bestmention.classifier.eval

import java.io.File
import edu.knowitall.main.FullDocSerializer
import edu.knowitall.tool.bestmention.classifier._
import edu.knowitall.common.Resource.using

object BestMentionClassifierAnalysis {

  def main(args: Array[String]): Unit = {

    val trainingFile = new File(args(0))
    val testFile = new File(args(1))
    val trainingDocsPath = new File(args(2))
    val testDocsPath = new File(args(3))
    val prOutFile = new File(args(4))
    val dataOutFile = new File(args(5))
    require(trainingFile.exists() && trainingFile.isFile(), "Invalid training file")
    require(testFile.exists() && testFile.isFile(), "Invalid test file")
    require(trainingDocsPath.exists() && trainingDocsPath.isDirectory(), "Invalid training doc path")
    require(testDocsPath.exists() && testDocsPath.isDirectory(), "Invalid test doc path")
    val trainingReader = LabelledResolvedBestMentionReader(trainingFile, trainingDocsPath)
    val testReader = LabelledResolvedBestMentionReader(testFile, testDocsPath)
    val classifier = BestMentionClassifier(trainingReader)
    val confidences = {
      testReader.labelledResolvedBestMentions
      .map(labelled => (classifier.apply(labelled.item), labelled))
      .toSeq
    }
    val sortedConfs = confidences.sortBy({case (conf, labelled) => -conf})
    val precs = precRecall(sortedConfs.map(_._2.label))
    val zipped = {
      precs.zip(sortedConfs)
      .map({case (prec, (conf, labelled)) => (prec, conf, labelled)})
      .toSeq
    }
    // sort for display purposes - by conf but put all the correct answers at the top (for p/r curve)
    val sortedZipped = zipped.sortBy({ case(prec, conf, labelled) => if (labelled.label) Double.MinValue else -conf })
    val dataOutLines = sortedZipped.map { case(prec, conf, labelled) =>
      Seq(
        "%.03f" format prec,               //precision
        "%.03f" format conf,               //confidence
        if (labelled.label) "1" else "0"   //label (1 or 0)
      ).mkString("\t") + ResolvedBestMentionWriter.writeRBM(labelled.item)
    }
    using(new java.io.PrintWriter(prOutFile)) { prOut =>
      removeSawtooth(precs)
      .map("%.03f".format(_))
      .map(prOut.println)
    }
    using(new java.io.PrintWriter(dataOutFile)) { dataOut =>
      val headers = Seq("prec", "conf") ++ ResolvedBestMentionWriter.featureHeaders
      dataOut.println(headers.mkString("\t"))
      dataOutLines.map(dataOut.println)
    }
    println("Feature weights:")
    classifier.featureWeights.foreach { case (feature, weight) =>
      println(s"$feature: $weight")
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
    result.reverse
  }

  def removeSawtooth(precs: List[Double]) = precs.tails.filter(_.nonEmpty).toList.map { tail => tail.max }

}