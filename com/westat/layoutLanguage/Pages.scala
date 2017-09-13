package com.westat.layoutLanguage

/**
 * Created by Owner on 9/1/2017.
 */
import com.westat.Length
import scala.collection.mutable.ListBuffer

case class Page(name : String, width : Length, height : Length, color : String) {
  private val list = new ListBuffer[Group]
  def addGroup(value : Group) : Group = {
    list += value
    value
  }
  def displayChildren = {
    println(this)
    list.foreach(g => g.displayChildren)
  }
  def toSVG : String = {
    val sb = new StringBuilder(
      s"""<svg id="$name" width="${width.asInchesString}" height="${height.asInchesString}"
      zoomAndPan="magnify" preserveAspectRatio="xMidYMid meet"
      xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">\n""")
    //    s"""<svg id="$name" width="${width.asDeviceUnits}" height="${height.asDeviceUnits}"
    //    zoomAndPan="magnify" preserveAspectRatio="xMidYMid meet"
    //    viewbox="0 0 ${width.asDeviceUnits} ${height.asDeviceUnits}"
    //    xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">\n""")
    sb.append(s"""<rect x="0" y="0" width="${width.asInchesString}" height="${height.asInchesString}" fill="$color"/>\n""")
    list.foreach(g => sb.append(g.toSVG))
    sb.append("</svg>")
    sb.toString()
  }
}

object Pages {
  private val list = new ListBuffer[Page]
  list += Page("Letter portrait", Length.dimension("8.5in"), Length.dimension("11in"), "lightgreen")
  list += Page("Letter landscape", Length.dimension("11in"), Length.dimension("8.5in"), "orange")

  def pageForName(aName : String) : Option[Page] = {
    list.find(p => p.name == aName)
  }
}
