package com.westat.layoutLanguage

import scala.collection.mutable.ListBuffer

/**
 * Created by lee on 6/30/17.
 */
trait Container {
  protected val children = new ListBuffer[GraphicItem]
  def addItem(value : GraphicItem) : Container = {
    children += value
    this
  }
  def displayChildren = {
    println(this)
    children.foreach(c => println(c))
  }
  def toSVG : String = {
    val sb = new StringBuilder(toString+"\n")
    children.foreach(c => sb.append(c.toSvg(null)))
    sb.toString
  }
}
