package com.westat

import scala.collection.mutable.ListBuffer

/**
  * Created by lee on 8/8/17.
  */
case class PropertyFile(filename : String) {
  private val list = new ListBuffer[(String, String)]
  load

  private def load : PropertyFile = {
    val src = scala.io.Source.fromFile(filename).getLines()
    src.foreach(p => {
       val kv = p.split('=')
       list += kv(0) -> kv(1)
    })
    this
  }

  def getValue(key : String) : Option[String] = {
    val idx = list.indexWhere(a => a._1 == key)
    if (idx >= 0)
      Some(list(idx)._2.toString)
    else
      None
  }

  def value(key : String) : String = {
    getValue(key) match {
      case Some(aval) => aval
      case None => ""
    }
  }

  def show = {
    list.foreach(p => println(p))
  }

}
