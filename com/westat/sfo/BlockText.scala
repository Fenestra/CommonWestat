package com.westat.sfo

import com.westat.gids.GidsFont
import com.westat.{BoxPoints, Length, Location, StringUtilities}
import scala.collection.mutable.ListBuffer

/**
  * Created by lee on 5/24/17.
  *
  * Notes on WordWrapping:
  *
  * PageBlocks are essentially paragraphs. They hold one or more InlineText objects that contain the text.
  * After each block is rendered, it should therefore produce a line spacer to separate paragraphs.
  *
  * Multiple InlineTexts within a block usually means that there is different formatting for each part
  * of the text.  Text1 should be printed, then Text2 should be printed on the same line if possible,
  * but containing different font characteristics.  Text3 should be printed on the same line as Text2
  * if possible, etc... If printing the text would overflow the current line, it should only print out
  * what can reasonably fit, then continue the remainder of the text on the following line.
  *
  * A block with only one InlineText that is very long should only print out what it can fit on a line,
  * then continue printing out the remainder on additional lines.
  *
  * Text output is controlled by the PageBlock, since it knows what the default font should be and
  * what dimensions the text should fit in.  Wordwrapping starts by calculating an approximation of
  * the number of characters that can fit on a line using the current font.  We then calculate the
  * actual dimensions the text would occupy.  If the text can fit, we simply output the text.
  *
  * If the text will not on a line, we break the text up into an array of strings.  The first string
  * will have a length that should fit in the space remaining on the current line.  Additional strings
  * will have a maximium character count that was calculated for that font.
  *
  * When a piece of text must be broken up into an array, the array is printed out using pretty much the
  * same restrictions.  If each string can fit in the available space on the current line, it is printed.
  * Otherwise, the string is printed on the next line starting at the left side of the available space.
  *
  *      0                        1.5" 1.6"                             4" 4.1"                         6"
  *   2" The first text to be printed  IS FOLLOWED BY DIFFERENT FORMATTING, then the default font is
  * 2.1" resumed and additional text is printed on the next line.
  *
  *      0" is the left bounds of this text space
  *      1.5" is the space used by Text1
  *      1.6" is the location where Text2 starts printing
  *      4" is where Text2 ends, so it occupied 4"-1.6" or 2.4"
  *      4.1" is the location where Text3 starts printing
  *
  *      2" is top location for the first line of the block
  *      After Text2 was output, the first line only has 1.9" of available space.
  *      Text3 would use up about 5.5", and would overflow the first line.  It is
  *      therefore broken up into a string that is less than 2" and addional strings
  *      that can fit within the available width of 6".
  *      4.1" left and 2" top is where the first part of Text3 is printed
  *      0" left and 2.1" top is where the next part of Text3 is printed

This is a
bold
statement.
 figure out length for all 3 = totalLength
 firstLength secondLength, thirdLength
left justify
 first text left
 second text left + firstLength
 third text left + firstLength + secondLength
center justify
 manually figure out left   spacer is (totalWidth - totalLength) / 2
 or figure out via middle justify
right justify

*/

//text-align left, center, right => text-anchor=start, middle, or end
case class BlockText(font : GidsFont, textAlign : TextAlignments.Value) extends PageBlock {
  private var textList = new ListBuffer[InlineText]
  private var ourLocation : Location = _
  private var availRect : Location = _

  var bottom = Length.dimension("0fu")

  def isEmpty : Boolean = {
    textList.isEmpty || (
      (textList.length == 1) && textList.head.isEmpty
    )
  }

  def addText(value : InlineText) : BlockText = {
    textList += value
    this
  }

  def displayString : String = {
    val sb = new StringBuilder(toString+"\n")
    textList.foreach(t => sb.append("      "+t.toString+"\n"))
    sb.toString()
  }

  private def bottomBounds : Length = ourLocation.bottom
  private def lineSize : Length = font.rawSize

  private def currentFont(textfont : GidsFont) : GidsFont = {
    if (textfont == null)
      font
    else
      textfont
  }

  private def output(sb : StringBuilder, sometext : String, startRect : Location, usedLength : Length, fs : String) : Location = {
    val lineX = xForAlignment(startRect).asInchesString
    val line = s"""<tspan x="$lineX" y="${startRect.top.asInchesString}" $fs>$sometext</tspan>\n"""
    sb.append(line)
    calcNextPosition(startRect, usedLength)
  }

  private def processOutput(sb : StringBuilder, aList : List[String], startRect : Location, aFont : GidsFont, fs : String) : Location = {
    var nextRect = startRect
    if (nextRect.top >= bottomBounds)
      return nextRect
    val iter = aList.iterator
    while (iter.hasNext) {
      val ttext = iter.next()
      if (iter.hasNext)
        // no need to calculate space used, just send entire width of line
        nextRect = output(sb, ttext, nextRect, nextRect.width, fs)
      else {
        // this is the last line and probably doesnt occupy the entire width
        val usedLength = aFont.stringWidth(ttext)
        nextRect = output(sb, ttext, nextRect, usedLength, fs)
      }
    }
    nextRect
  }

  private def addMultipleStrings(sb : StringBuilder, sometext : String, startRect : Location, aFont : GidsFont, fs : String) : Location = {
    if (startRect.top >= bottomBounds)
      return startRect
    val lines = StringUtilities.fitStringToLengths(sometext, startRect.width, ourLocation.width, aFont)
    val nextRect = output(sb, lines.head, startRect, startRect.width, fs)
    processOutput(sb, lines.tail, nextRect, aFont, fs)
  }

  private def xForAlignment(aRect : Location) : Length = {
     textAlign match {
       case TextAlignments.taLeft => aRect.left
       case TextAlignments.taCenter => aRect.left + (aRect.width / 2)
       case TextAlignments.taRight  => aRect.right
     }
  }

  private def wrapToNewLine(startRect : Location) : Location = {
    Location.create(ourLocation.left, startRect.top + lineSize, ourLocation.width, startRect.height - lineSize)
  }

  private def calcNextPosition(startRect : Location, usedLength : Length) : Location = {
    val newWidth = startRect.width - usedLength
    if (newWidth <= Length.dimension("5fu"))
      return wrapToNewLine(startRect)
    textAlign match {
      case TextAlignments.taLeft =>
        Location.create(startRect.left + usedLength, startRect.top, newWidth, startRect.height)
      case TextAlignments.taCenter => // treat it like leftAlign as temporary fix
        Location.create(startRect.left + usedLength, startRect.top, newWidth, startRect.height)
      case TextAlignments.taRight  =>
        Location.create(startRect.left, startRect.top, newWidth, startRect.height)
    }
  }

  // pageblock knows everything about the space to draw, so it controls drawing
  // go through each inline text and draw it
  def toSVG(location : Location, paragraphs : Boolean) : String = {
    // initialize available and used positions
    availRect = startPrinting(location)
    // start the string builder
    val sb = new StringBuilder(s"""<text x="${availRect.left.asInchesString}" y="${availRect.top.asInchesString}" ${font.asSVGString} text-anchor="${textAlign}">\n""")
    // process each piece of text
    textList.foreach(t => {
      availRect = printText(sb, t, availRect)
    })
    if (paragraphs)
      bottom = availRect.top + lineSize
    else
      bottom = availRect.top
    // complete the string and return the result
    sb.append("</text>\n")
    sb.toString()
  }

  private def startPrinting(location : Location) : Location = {
    ourLocation = location.copyOf
    Location.create(location.left, location.top + font.lineSpace, location.width, location.height)
  }

  def printText(sb : StringBuilder, t : InlineText, startRect : Location) : Location = {
    val ttext = t.text.trim
    val thisLength = currentFont(t.font).stringWidth(ttext)
    if (thisLength <= startRect.width) {
      // just output the text in the available space
      output(sb, ttext, startRect, thisLength, t.fontstring)
    }
    else
      // this text requires multiple lines
      addMultipleStrings(sb, ttext, startRect, currentFont(t.font), t.fontstring)
  }

}

