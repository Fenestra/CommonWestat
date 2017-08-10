package com.westat.sfo

import com.westat.gids.GidsFont
import com.westat.{Length, Location, StringUtilities}
import scala.collection.mutable.ListBuffer

/**
  * Created by lee on 7/31/17.
  *
  * To correctly handle text justification of Centered text in all situations, it was necessary
  * to break down (or assemble) all the text that should appear on lines of a specified width.
  * When lines consist of pieces of text with different font characteristics, we must handle the
  * Centered text by calculating the middle point of each separate piece of text.  The process is:
  *
  * Centered Text:
  * Calculate the starting offset = (maximum line width) - (total width of text) / 2
  *   this offset puts the same amount of white space at start and end of the line
  * when each line is output to SVG, each piece of text is made to center at the
  * midpoint of its own width plus the left origin for its output rectangle.
*/

case class LinePiece(text : String, length : Length, fs : String)

case class OutputLine(length : Length, textAlign : TextAlignments.Value) {
  private var list = new ListBuffer[LinePiece]

  def totalWidth : Length = {
    var x = Length.dimension("0fu")
    list.foreach(lp => x = x + lp.length)
    x
  }

  def remainingWidth : Length = {
    length - totalWidth
  }

  def addText(text : String, length : Length, fs : String) : OutputLine = {
    if (textAlign == TextAlignments.taRight)
      list = LinePiece(text, length, fs) +: list
    else
      list += LinePiece(text, length, fs)
    this
  }

  def contents : String = {
    val sb = new StringBuilder()
    list.foreach(lp => sb.append(lp.text + " "))
    sb.toString
  }

  def toSVG(sb : StringBuilder, startRect : Location, fs : String) = {
    var rect = startRect.copyOf
    val lineX = xForAlignment(rect)
    val startfs = list.head.fs
    val line = s"""<tspan x="${lineX.asInchesString}" y="${startRect.top.asInchesString}">\n"""
    sb.append(line)
    list.foreach(p => {
      if (p.fs != fs)
        sb.append(s"""<tspan ${p.fs}>${p.text}</tspan>\n""")
      else
        sb.append(p.text)
    })
    sb.append("</tspan>\n")
  }

  private def xForAlignment(aRect : Location) : Length = {
    textAlign match {
      case TextAlignments.taLeft => aRect.left
      case TextAlignments.taCenter => aRect.left + (aRect.width / 2)
      case TextAlignments.taRight  => aRect.right
    }
  }

}


//text-align left, center, right => text-anchor=start, middle, or end
case class LineListFactory(textList : List[InlineText], maxWidth : Length, font : GidsFont, textAlign : TextAlignments.Value) {
  private var linelist = getLineList
  private def lineSize : Length = font.rawSize

  def lines(newWidth : Length = maxWidth) : List[OutputLine] = {
    if (newWidth != maxWidth) {
      LineListFactory(textList, newWidth, font, textAlign).lines()
    }
    else
      linelist.toList
  }

  private def currentFont(textfont : GidsFont) : GidsFont = {
    if (textfont == null)
      font
    else
      textfont
  }

  private def getLineList : ListBuffer[OutputLine] = {
    linelist = new ListBuffer[OutputLine]
    var thisWidth : Length = null
    var lineObj = addLine
    textList.foreach(t => {
      thisWidth = currentFont(t.font).stringWidth(t.text)
      lineObj = addLineIfNeeded(lineObj)
//    println(s" this text ${t.text} len=$thisWidth remaining:${lineObj.remainingWidth}")
      if (thisWidth <= lineObj.remainingWidth)
        lineObj = lineObj.addText(t.text, thisWidth, t.fontstring)
      else
        lineObj = addMultipleLines(lineObj, t, t.fontstring)
    })

    linelist
  }

  private def addLine : OutputLine = {
    val obj = OutputLine(maxWidth, textAlign)
    linelist += obj
    obj
  }

  private def addLineIfNeeded(line : OutputLine) : OutputLine = {
    if (line.remainingWidth < Length.dimension(".05in"))
      addLine
    else
      line
  }

  private def addMultipleLines(lineObj : OutputLine, t : InlineText, fs : String) : OutputLine = {
    val thisFont = currentFont(t.font)
    val lines = StringUtilities.fitStringToLengths(t.text, lineObj.remainingWidth, maxWidth, thisFont)
    var thisWidth = thisFont.stringWidth(lines.head)
    var result = lineObj.addText(lines.head, thisWidth, fs)
    val iter = lines.tail.iterator
    while (iter.hasNext) {
      val ttext = iter.next()
      if (iter.hasNext)
      // no need to calculate space used, just send entire width of line
        result = addLine.addText(ttext, result.length, fs)
      else {
        // this is the last line and probably doesnt occupy the entire width
        thisWidth = thisFont.stringWidth(ttext)
        result = addLine.addText(ttext, thisWidth, fs)
      }
    }
    result
  }

}

