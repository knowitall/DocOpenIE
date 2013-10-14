package edu.knowitall.tool.link

import edu.knowitall.repr.coref.Mention
import edu.knowitall.repr.coref.Linked
import edu.knowitall.repr.document.Document

trait Linker {
  def link(mention: Mention, doc: Document): Seq[Mention with Linked]
}