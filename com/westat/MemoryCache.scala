package com.westat

import scala.collection.mutable.ListBuffer

// Created by lee on 8/24/17.

case class MemoryCache(extension : String) {
  private val list = new ListBuffer[(String, String)]

  def find(id : String) : Option[String] = {
    val idx = list.indexWhere(a => a._1 == id)
    if (idx >= 0)
      Some(list(idx)._2)
    else
      None
  }

  def write(id : String, contents : String) = {
    val idx = list.indexWhere(a => a._1 == id)
    if (idx >= 0)
      list.update(idx, id -> contents)
    else
      list += id -> contents
  }

  private def prepareForWriting : MemoryCache = {
    if (list.length > 30)
      list.clear()
    this
  }
}

case class MemoryBACache(extension : String) {
  private val list = new ListBuffer[(String, Array[Byte])]

  def find(id : String) : Option[Array[Byte]] = {
    val idx = list.indexWhere(a => a._1 == id)
    if (idx >= 0)
      Some(list(idx)._2)
    else
      None
  }

  def write(id : String, contents : Array[Byte]) = {
    val idx = list.indexWhere(a => a._1 == id)
    if (idx >= 0)
      list.update(idx, id -> contents)
    else
      list += id -> contents
  }

  def prepareForWriting : MemoryBACache = {
    if (list.length > 30)
      list.clear()
    this
  }
}

object MemoryCache {
  private val svgObj = MemoryCache("svg")
  private val pdfObj = MemoryBACache("pdf")

  def svgCache : MemoryCache = {
    svgObj.prepareForWriting
  }

  def pdfCache : MemoryBACache = {
    pdfObj.prepareForWriting
  }
}