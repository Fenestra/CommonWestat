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

  def toSVG(sb : StringBuilder, startRect : Location, fs : String) = {
//    println(s"outputline has start of $startRect")
    var rect = startLine(startRect, list.head.length)
    list.foreach(p => {
      var lineX = xForAlignment(rect, p.length)
      val line = s"""<tspan x="${lineX.asInchesString}" y="${startRect.top.asInchesString}" ${p.fs}>${p.text}</tspan>\n"""
      rect = calcNextPosition(rect, p.length)
      sb.append(line)
    })
  }

  def startLine(loc : Location, firstLen : Length) : Location = {
    if ((list.length == 1) || (textAlign != TextAlignments.taCenter))
      return loc.copyOf
    val offset = (loc.width - totalWidth) / 2
    Location.create(loc.left + offset, loc.top, firstLen, loc.height)
  }

  private def xForAlignment(aRect : Location, twidth : Length) : Length = {
    textAlign match {
      case TextAlignments.taLeft => aRect.left
      case TextAlignments.taCenter => if (list.length == 1)
        aRect.left + (aRect.width / 2)
       else
        aRect.left + (twidth / 2)
      case TextAlignments.taRight  => aRect.right
    }
  }

  private def calcNextPosition(startRect : Location, textLen : Length) : Location = {
    val newWidth = startRect.width - textLen
    textAlign match {
      case TextAlignments.taLeft =>
        Location.create(startRect.left + textLen, startRect.top, newWidth, startRect.height)
      case TextAlignments.taCenter =>
        Location.create(startRect.left + textLen, startRect.top, newWidth, startRect.height)
      case TextAlignments.taRight  =>
        Location.create(startRect.left, startRect.top, newWidth, startRect.height)
    }
  }
}

//text-align left, center, right => text-anchor=start, middle, or end
case class LineListFactory(textList : List[InlineText], maxWidth : Length, font : GidsFont, textAlign : TextAlignments.Value) {
  private var linelist = getLineList
  private def lineSize : Length = font.rawSize

  def lines : List[OutputLine] = linelist.toList

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
      if (thisWidth < lineObj.remainingWidth)
        lineObj.addText(t.text, thisWidth, t.fontstring)
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
    if (line.remainingWidth < Length.dimension(".18in"))
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
    lineObj
  }

}

