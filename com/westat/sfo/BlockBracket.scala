package com.westat.sfo

import com.westat.gids.GidsFont
import com.westat.{Length, Location, StringUtilities}
import scala.collection.mutable.ListBuffer

/**
  * Created by lee on 5/24/17.
**/

case class BlockBracket(direction : String, lineWidth : Length, outlineColor : String, size : Length) extends PageBlock {
  def bottom : Length = Length.dimension("0fu")

  def displayString : String = {
    toString+"\n"
  }

  def isEmpty : Boolean = false

  def toSVG(location: Location, paragraphs: Boolean): String = {
    val acolor = if (outlineColor.nonEmpty)
      outlineColor
    else
      "blue"
    val sb = new StringBuilder(
      s"""<svg id="bracket-$direction" x="${location.left.asInchesString}" y="${location.top.asInchesString}"
         width="${location.width.asInchesString}" height="${location.height.asInchesString}">\n""")

    val scale = 1
    val angle = direction match {
      case "east"  => 180
      case "north" => 90
      case "west"  => 0
      case "south" => 270
      case _  => 0
    }
    // <g  transform="scale(0.5) rotate(180 50,50)" >
    sb.append(s""" <g transform="scale($scale) rotate($angle 50,50)">\n""")
//    sb.append(s"""<rect x="0" y="0" width="100" height="100" fill="pink" />\n""")
    sb.append(s"""<text x="50" y="75"
       style="font-size:66pt;stroke:none;fill:$acolor;
       font-weight:lighter;font-family:Arial Narrow;font-stretch:ultra-condensed;
       text-anchor:middle;glyph-orientation-horizontal:0;">{</text>\n""")
    sb.append("</g>\n")
    sb.append("</svg>\n")
    sb.toString()
  }
}
