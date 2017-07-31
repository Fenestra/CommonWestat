package com.westat.sfo

import com.westat.gids.GidsFont
import com.westat.{Length, Location, StringUtilities}
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
  * A LineList is created that breaks up all the text into lines that will fit into the specified line
  * size.  Each OutputLine will have 1 to many pieces of text that are contained on that line.  Therefore,
  * justification can be controlled by the OutputLine whether Left, Centered, or Right justified.
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
  *   2.1" resumed and additional text is printed on the next line.
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

*/

//text-align left, center, right => text-anchor=start, middle, or end
case class BlockText(font : GidsFont, textAlign : TextAlignments.Value) extends PageBlock {
  private var textList = new ListBuffer[InlineText]
  private var lineList = List[OutputLine]()
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

  private def lineSize : Length = font.rawSize

  private def currentFont(textfont : GidsFont) : GidsFont = {
    if (textfont == null)
      font
    else
      textfont
  }

  // pageblock knows everything about the space to draw, so it controls drawing
  // go through each inline text and draw it
  def toSVG(location : Location, paragraphs : Boolean) : String = {
    // initialize available and used positions
    availRect = startPrinting(location)
    // start the string builder
    val sb = new StringBuilder(s"""<text x="${availRect.left.asInchesString}" y="${availRect.top.asInchesString}" ${font.asSVGString} text-anchor="${textAlign}">\n""")

    // gather all the lines
    lineList = LineListFactory(textList.toList, location.width, font, textAlign).lines
    // process each line
    lineList.foreach(line => {
      line.toSVG(sb, availRect, font.asSVGString)
      availRect = wrapToNewLine(availRect)
    })

    if (paragraphs)
      bottom = availRect.top + lineSize
    else
      bottom = availRect.top
    // complete the string and return the result
    sb.append("</text>\n")
    sb.toString()
  }

  private def wrapToNewLine(startRect : Location) : Location = {
    Location.create(ourLocation.left, startRect.top + lineSize, ourLocation.width, startRect.height - lineSize)
  }

  private def startPrinting(location : Location) : Location = {
    ourLocation = location.copyOf
    Location.create(location.left, location.top + font.lineSpace, location.width, location.height)
  }
}

