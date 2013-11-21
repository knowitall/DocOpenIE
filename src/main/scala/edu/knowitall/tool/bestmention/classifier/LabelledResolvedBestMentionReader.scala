package edu.knowitall.tool.bestmention.classifier

import java.io.File
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.repr.bestmention.ResolvedBestMention
import edu.knowitall.main.FullDocSerializer
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing

case class LabelledResolvedBestMentionReader(trainingFile: File, docsPath: File) extends Iterable[Labelled[ResolvedBestMention]] {

  require(trainingFile.exists() && trainingFile.isFile(), "Invalid training file")
  require(docsPath.exists() && docsPath.isDirectory(), "Invalid doc path")
  
  // A map from DocId to Doc
  val docsMap = docsPath.listFiles().map(FullDocSerializer.deserializeFromFile).map(d => (d.docId -> d)).toMap
  
  // a list of (label, doc Index, doc Id)
  val labelIndexId = using(io.Source.fromFile(trainingFile)) { trainingSource =>
    trainingSource.getLines
    .drop(1)
    .map(_.split("\t"))
    .filter(_.length > 3)
    .map(s => (s(0), s(s.length-2), s(s.length-1)))
    .collect({
       case ("1", indexStr, docIdStr) => (true, indexStr.toInt, docIdStr) 
       case ("0", indexStr, docIdStr) => (false, indexStr.toInt, docIdStr)
    })
    .toList
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