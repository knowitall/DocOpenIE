package edu.knowitall.repr.document

trait DocId {
  this: Document =>
  
  def docId: String
  
}