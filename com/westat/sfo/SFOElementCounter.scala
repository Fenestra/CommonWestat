
package com.westat.sfo

import scala.xml.{XML, Node}

case class ElementCounter() {
  private val map = scala.collection.mutable.HashMap.empty[String, Int]

  def countElements(node: scala.xml.Node): ElementCounter = {
    val ars = node.child
    ars.foreach(a => {
      val name = if (a.label == "block-graphic")
        a.label + " " + (a \ "@graphic-class").text
      else
        a.label
      val cnt = map.getOrElse(name, 0)
      map += (name -> (cnt + 1))
      countElements(a)
    })
    this
  }

  def showResults: String = {
    map.toList.sortBy(e => e._1).mkString("\n")
  }

}

object ElementCounter {
  val ignoreList = List("layout-master-set",
    "page-master",
    "media",
    "region",
    "area",
    "inline",
    "page-sequence",
    "sequence-specification",
    "sequence-specifier-single",
    "#PCDATA",
    "flow" )
  def shouldCount(value : String) : Boolean = {
//    println(s"has $value "+ ignoreList.contains(value).equals(false))
    ignoreList.contains(value).equals(false)
  }
}

case class SFOElementCounter(text : String) {
  private var xml : Node = null
  def readFromFile : Node = {
    xml = XML.loadFile(text).head
    xml
  }
/*
layout-master-set
page-master
media
region
area
page-sequence
sequence-specification
sequence-specifier-single
inline
block-graphic  check what (n \"@graphic-class").text we have
block-bracket
block-box
block
flow

we test these but dont really do anything with them
block-check-box
block-answer-box

*/
}

object SFOElementCounter {
  def test = {
    val counter = ElementCounter()
    counter.countElements(SFOElementCounter("instructions.sfo").readFromFile)
    counter.countElements(SFOElementCounter("BracketSFO.xml").readFromFile)
    counter.countElements(SFOElementCounter("Converted-documentLayout.txt").readFromFile)
    counter.showResults
  }

}

