package edu.knowitall.tool.bestmention.classifier

import java.io.File
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.repr.bestmention.ResolvedBestMention
import edu.knowitall.main.FullDocSerializer
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing

case class LabelledResolvedBestMentionReader(trainingFile: File, docsPath: File) extends Iterable[Labelled[ResolvedBestMention]] {

  require(trainingFile.exists() && trainingFile.isFile(), "Invalid training file")
  require(docsPath.exists() && docsPath.isFile(), "Invalid doc path")
  
  // A map from DocId to Doc
  val docsMap = docsPath.listFiles().map(FullDocSerializer.deserializeFromFile).map(d => (d.docId -> d)).toMap
  
  // a list of (label, doc Index, doc Id)
  def labelIndexId = using(io.Source.fromFile(trainingFile)) { trainingSource =>
    trainingSource.getLines
    .map(_.split("\t"))
    .filter(_.length > 3)
    .map(s => (s(0), s(s.length-2), s(s.length-1)))
    .map({case (labelStr, indexStr, docIdStr) => (labelStr == "1", indexStr.toInt, docIdStr) })
  }

  val labelledResolvedBestMentions = {
    val (nsResult, result) = Timing.time {
      labelIndexId.toIterable
      .flatMap { case (label, index, docId) =>
        docsMap.get(docId)
        .flatMap(_.bestMentions.lift(index))
        .map(bm => (Labelled(label, bm), index, docsMap(docId)))
      }
    }
    System.err.println(s"Loaded ${result.size} labeled examples in ${Timing.Seconds.format(nsResult)}.")
    result
  }
  
  def iterator = labelledResolvedBestMentions.iterator.map(_._1)
}