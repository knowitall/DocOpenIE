package edu.knowitall.repr.document

import edu.knowitall.repr.coref.MentionCluster
import edu.knowitall.repr.coref.Mention

trait CorefResolved[M <: Mention] {
  this: Document =>

  def clusters: Seq[MentionCluster[M]]
}