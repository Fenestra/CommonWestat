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
  */

case class LayoutPoint(left : Length, top : Length)

case class BoxPoints(xLeft : Length, xMiddle : Length, xRight : Length, yTop : Length,
                     yMiddle : Length, yBottom : Length, rx : Length, ry : Length)

object TextAlignments extends Enumeration {
  val taLeft = Value("start")
  val taCenter = Value("middle")
  val taRight = Value("end")
  def valueForTextAlignString(s : String) : TextAlignments.Value = {
    s match {
      case "center" => taCenter
      case "right"  => taRight
      case _        => taLeft
    }
  }
}

object BoxStyles extends Enumeration {
  val bsTriangleEast  = Value("triangle-east")
  val bsTriangleNorth = Value("triangle-north")
  val bsTriangleWest  = Value("triangle-west")
  val bsTriangleSouth = Value("triangle-south")
  val bsRectangle     = Value("rectangle")
  val bsDiamond       = Value("diamond")
  val bsEllipse       = Value("ellipse")
  def valueForStyleString(s : String) : BoxStyles.Value = {
    s match {
      case "triangle-east"  => bsTriangleEast
      case "triangle-north" => bsTriangleNorth
      case "triangle-west"  => bsTriangleWest
      case "triangle-south" => bsTriangleSouth
      case "diamond"        => bsDiamond
      case "ellipse"        => bsEllipse
      case _                => bsRectangle
    }
  }
}

object GraphicKinds extends Enumeration {
  val gkBitmap     = Value("bmp")
  val gkJPEG       = Value("jpg")
  val gkPNG        = Value("png")
  val gkMetafile   = Value("emf")
  val gkBarcode    = Value("barcode")
  val gkEyeReadableNumber = Value("eye-readable-number")
  val gkRotatedText = Value("rotated")
  def valueForKindString(s : String) : GraphicKinds.Value = {
    s match {
      case "bmp"     => gkBitmap
      case "jpg"     => gkJPEG
      case "png"     => gkPNG
      case "emf"     => gkMetafile
      case "barcode" => gkBarcode
      case "eye-readable-number" => gkEyeReadableNumber
      case "rotated" => gkRotatedText
      case _         => gkPNG
    }
  }
}

case class InlineText(text : String, font : GidsFont) {
  private val maxLength = Length.dimension("7in")
  var lineCount = 1
  var usedLength = Length.dimension("0fu")
  var startLeft = Length.dimension("0fu")
  var nextLeft = Length.dimension("0fu")
  var availableWidth = Length.dimension("0fu")
  var isOversize : Boolean = false
  var isEmpty : Boolean = text.length == 0

  def fontstring : String = {
    if (font.ne(null))
      font.asSVGString
    else
      ""
  }
}

trait PageBlock {
  def bottom : Length
  def displayString : String
  def toSVG(location: Location, paragraphs: Boolean): String
  def isEmpty : Boolean
}

//text-align left, center, right => text-anchor=start, middle, or end
case class BlockText(font : GidsFont, textAlign : TextAlignments.Value) extends PageBlock {
  private var textList = new ListBuffer[InlineText]
  private var ourLocation : Location = _

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

  private def leftBounds : Length = ourLocation.left
  private def rightBounds : Length = ourLocation.right
  private def bottomBounds : Length = ourLocation.bottom
  private def lineSize : Length = font.rawSize

  private def currentFont(textfont : GidsFont) : GidsFont = {
    if (textfont == null)
      font
    else
      textfont
  }


  private def output(sb : StringBuilder, sometext : String, x : String, y : String, fs : String, usedLength : Length) = {
    val lineX = xForAlignment(Length.dimension(x), usedLength).asInchesString
    val line = s"""<tspan x="$lineX" y="$y" $fs>$sometext</tspan>\n"""
    StringUtilities.debugln(s" - - - x=$x y=$y $sometext")
    sb.append(line)
  }

  private def processOutput(sb : StringBuilder, sometext : String, position : LayoutPoint, aFont : GidsFont, fs : String) : LayoutPoint = {
    var aLeft = position.left
    var aTop = position.top
    if (aTop > bottomBounds)
      return position
    var usedLength = currentFont(aFont).stringWidth(sometext)
    if ((usedLength + aLeft) < rightBounds) {
        output(sb, sometext, aLeft.asInchesString, aTop.asInchesString, fs, usedLength)
        aLeft = aLeft + usedLength
      }
      else { // force a new line and start from the beginning
        aTop = aTop + lineSize
        if (aTop > bottomBounds)
          return position
        output(sb, sometext, leftBounds.asInchesString, aTop.asInchesString, fs, usedLength)
        aLeft = leftBounds + usedLength
      }
    LayoutPoint(aLeft, aTop)
  }

  private def addMultipleStrings(sb : StringBuilder, sometext : String, x : Length, y : Length, aFont : GidsFont, fs : String) : LayoutPoint = {
    val lines = StringUtilities.fitStringToLengths(sometext, rightBounds - x, rightBounds - leftBounds, aFont)
    val usedLength = currentFont(aFont).stringWidth(lines.head)
    if (y > bottomBounds)
      return LayoutPoint(x, y)
    output(sb, lines.head, x.asInchesString, y.asInchesString, fs, usedLength)

    var aLeft = leftBounds
    var aTop = y + lineSize
    lines.tail.foreach(s => {
        //      need x, dy, text, charsAvail,   hold left and usedLength so we can calc nextLeft   pass something that tells this is last line, so nextleft is left
        val newpos = processOutput(sb, s, LayoutPoint(aLeft, aTop), aFont, fs)
        aLeft = newpos.left
        aTop = newpos.top
      })
    LayoutPoint(aLeft, aTop)
  }

  def xForAlignment(aLeft : Length, used : Length) : Length = {
     textAlign match {
       case TextAlignments.taLeft => aLeft
       case TextAlignments.taCenter => aLeft + (rightBounds - aLeft) * 0.5
       case TextAlignments.taRight  => rightBounds
     }
  }

  // pageblock knows everything about the space to draw, so it controls drawing
  // go through each inline text and draw it
  def toSVG(location : Location, paragraphs : Boolean) : String = {
    ourLocation = location.copyOf
//    println(s"  PageBlock started $location")
    val x = location.left.asInchesString
    val y = location.top.asInchesString
StringUtilities.debugln(s"pageblock toSVG called with top of $y and our loc:${ourLocation.rectString}")
    var aLeft = leftBounds
    var aTop = location.top + font.lineSpace //lineSize
    var usedLength : Length = null
    val res = new StringBuilder(s"""<text x="$x" y="$y" ${font.asSVGString} text-anchor="${textAlign}">\n""")
//    val res = new StringBuilder(s"""<text x="$x" y="$y" style="white-space:pre" ${font.asSVGString}>\n""")
 if (isEmpty)
   println("------------------ pageblock is empty, but printing anyway -------------------------")
    textList.foreach(t => {
      val ttext = t.text.trim
      usedLength = currentFont(t.font).stringWidth(ttext)
      StringUtilities.debugln(s"pageblock printing text left:$aLeft  top:$aTop ")
      if ((usedLength + aLeft) <= rightBounds) {
        output(res, ttext, aLeft.asInchesString, aTop.asInchesString, t.fontstring, usedLength)
        aLeft = aLeft + usedLength
      }
      else {
        val newpos = addMultipleStrings(res, ttext, aLeft, aTop, currentFont(t.font), t.fontstring)
        aLeft = newpos.left
        aTop = newpos.top
      }
    })
    if (paragraphs)
      bottom = aTop + lineSize
    else
      bottom = aTop
    StringUtilities.debugln("================================ end of block and paragraph ====================")
    res.append("</text>\n")
    res.toString()
  }

}


case class BlockImage(graphicKind : GraphicKinds.Value, width : Length, height : Length, spaceBefore : Length, spaceAfter : Length, rawdata : String) extends PageBlock {
  def bottom : Length = Length.dimension("0fu")
  if (graphicKind != GraphicKinds.gkPNG)
    println(s"NOT A PNG $graphicKind ") //${rawdata.substring(1, 50)}")
  def data: String = {
    ImageData.blockDataToBase64ImageString(rawdata)
  }

  def displayString : String = {
    toString+"\n"
  }

  def isEmpty : Boolean = data.length < 100

  def toSVG(location: Location, paragraphs: Boolean): String = {
    graphicKind match {
       case GraphicKinds.gkPNG    => drawImage(location)
       case GraphicKinds.gkBitmap => drawImage(location)
       case GraphicKinds.gkJPEG   => drawImage(location)
       case _ => drawUnsupportedImage(location)
    }
  }

  def drawImage(location : Location) : String = {
    val sb = new StringBuilder(s"""<image id="$graphicKind-${StringUtilities.veryShortString(rawdata)}" x="${location.left.asInchesString}" y="${location.top.asInchesString}" """)
    sb.append(s"""width="${width.asInchesString}" height="${height.asInchesString}" """)
    sb.append(s"""xlink:href="data:image/$graphicKind;base64,$data" />\n""")
    sb.toString()
  }

  def drawUnsupportedImage(location : Location) : String = {
    var line = location.top
    val sb = new StringBuilder(s"""<text x="${location.left.asInchesString}" y="${line.asInchesString}" """)
    sb.append("""style="font-size:10pt;stroke:none;fill:black;font-weight:lighter;font-family:Arial Bold;font-stretch:ultra-condensed;">""")
    sb.append(s"""<tspan x="${location.left.asInchesString}" y="${line.asInchesString}" >($graphicKind)</tspan>\n""")
    line = line + Length.NEW_LINE_SIZE
    sb.append(s"""<tspan x="${location.left.asInchesString}" y="${line.asInchesString}">is not supported</tspan>\n</text>""")
    sb.toString()
  }
}

object BlockGraphic {
  def createGraphic(graphicClass : String, width : Length, height : Length, spaceBefore : Length, spaceAfter : Length, rawdata : String) : PageBlock = {
    val gKind = GraphicKinds.valueForKindString(graphicClass)
println(s"createGraphic got $graphicClass and turned it into $gKind")
    gKind match {
      case GraphicKinds.gkBarcode  => BlockBarcode(gKind, width, height, spaceBefore, spaceAfter, rawdata)
//      case GraphicKinds.gkPNG      => BlockImage(gKind, width, height, spaceBefore, spaceAfter, rawdata)
//      case GraphicKinds.gkBitmap   => BlockImage(gKind, width, height, spaceBefore, spaceAfter, rawdata)
//      case GraphicKinds.gkJPEG     => BlockImage(gKind, width, height, spaceBefore, spaceAfter, rawdata)
      case _ => BlockImage(gKind, width, height, spaceBefore, spaceAfter, rawdata)
    }
  }
}

case class BlockBracket(direction : String, lineWidth : Length, outlineColor : String, size : Length) extends PageBlock {
  def bottom : Length = Length.dimension("0fu")

  def displayString : String = {
    toString+"\n"
  }

  def isEmpty : Boolean = false

  def toSVG(location: Location, paragraphs: Boolean): String = {
    val asize = if (size != null)
       size.asInchesString
    else
       "40"
    val acolor = if (outlineColor.nonEmpty)
      outlineColor
    else
      "blue"
    val sb = new StringBuilder("") //s"""<rect id="bracket-$direction" x="${location.left.asInchesString}" y="${location.top.asInchesString}" """)
//    sb.append(s"""width="$asize" height="$asize" """)
//    sb.append(s"""width="${location.width.asInchesString}" height="${location.height.asInchesString}" """)
//    sb.append(s"""fill="$acolor" fill-opacity="0.95" />""")
    val aleft = location.left + Length.dimension("0.5in") //(location.width / 2)
    val atop  = location.top + Length.dimension("0.74in") //(location.height * 0.75)
    val trans = direction match {
//        case "east"  => "matrix(1 , 0 , 0 , 1 , 0 , 0 )"
//        case "north" => "matrix( 0, -1, 1, -0, -0.4817695617675781, 101.97134780883789)"
//        case "west"  => "matrix( -1, 0, 0, -1, 101.48958015441895, 102.45312690734863)"
//        case "south" => "matrix(0 1 1 0 1 -1)"
        case "south" => s"matrix(0 1 1 0 ${aleft.asInchesString} -${atop.asInchesString})"
        case _  => "matrix(1 , 0 , 0 , 1 , 0 , 0 )"
    }
 //   west glyph-orientation-horizontal:0;at end of style
    sb.append(s"""<text x="${aleft.asInchesString}" y="${atop.asInchesString}"
       style="font-size:66pt;stroke:none;fill:$acolor;
       font-weight:lighter;font-family:Arial Narrow;font-stretch:ultra-condensed;
       text-anchor:middle;glyph-orientation-vertical:90;" transform="$trans" >
      {</text>""")
    sb.toString()
  }
}

case class BlockBox(boxStyle : BoxStyles.Value, width : Length, height : Length, spaceBefore : Length, spaceAfter : Length,
                    lineWidth : Length, fillColor : String, outlineColor : String) extends PageBlock {
  def bottom : Length = Length.dimension("0fu")

  def displayString : String = {
    toString+"\n"
  }

  def isEmpty : Boolean = false

  def toSVG(location: Location, paragraphs: Boolean): String = {
    val loc = computeRectangle(location)
    val pts = computeBoxPoints(loc, width * 0.5, height * 0.4)

    val sb = new StringBuilder(s"""<g id="box-$boxStyle">\n""")
    val ss = s"fill:$fillColor;stroke-width:${lineWidth.asPoints};stroke:$outlineColor;"
    boxStyle match {
      case BoxStyles.bsTriangleEast => drawTriangleEast(sb, pts, ss)
      case BoxStyles.bsTriangleNorth => drawTriangleNorth(sb, pts, ss)
      case BoxStyles.bsTriangleWest => drawTriangleWest(sb, pts, ss)
      case BoxStyles.bsTriangleSouth => drawTriangleSouth(sb, pts, ss)
      case BoxStyles.bsRectangle => drawRectangle(sb, pts, ss)
      case BoxStyles.bsDiamond => drawDiamond(sb, pts, ss)
      case BoxStyles.bsEllipse => drawEllipse(sb, pts, ss)
    }
    sb.append(s"</g>\n")
    sb.toString()
  }

  private def computeRectangle(loc : Location) : Location = {
    Location.create(loc.left, loc.top + spaceBefore, width, height - spaceAfter)
  }

  private def computeBoxPoints(loc : Location, rx : Length, ry : Length) : BoxPoints = {
    val offset = Length.max((loc.right - loc.left - loc.width) * 0.5, Length.dimension("0fu"))
    val xl = loc.left + offset
    val xr = loc.right - offset
    val yt = loc.top
    val yb = loc.top + loc.height
    BoxPoints(xl, (xl + xr) * 0.5, xr, yt, (yt + yb) * 0.5, yb, rx, ry)
  }
  private def drawTriangleEast(sb : StringBuilder, pts : BoxPoints, styleString : String) = {
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yMiddle.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yMiddle.asInchesString}" x2="${pts.xLeft.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xLeft.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
  }
  private def drawTriangleNorth(sb : StringBuilder, pts : BoxPoints, styleString : String) = {
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yBottom.asInchesString}" x2="${pts.xMiddle.asInchesString}" y2="${pts.yTop.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xMiddle.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yBottom.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
  }
  private def drawTriangleWest(sb : StringBuilder, pts : BoxPoints, styleString : String) = {
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xLeft.asInchesString}" y2="${pts.yMiddle.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yBottom.asInchesString}" x2="${pts.xLeft.asInchesString}" y2="${pts.yMiddle.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
  }
  private def drawTriangleSouth(sb : StringBuilder, pts : BoxPoints, styleString : String) = {
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xMiddle.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xMiddle.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yTop.asInchesString}" style="$styleString" />\n""")
  }
  private def drawRectangle(sb : StringBuilder, pts : BoxPoints, styleString : String) = {
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yTop.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yBottom.asInchesString}" x2="${pts.xLeft.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yBottom.asInchesString}" x2="${pts.xLeft.asInchesString}" y2="${pts.yTop.asInchesString}" style="$styleString" />\n""")
  }
  private def drawDiamond(sb : StringBuilder, pts : BoxPoints, styleString : String) = {
    sb.append(s"""<line x1="${pts.xMiddle.asInchesString}" y1="${pts.yTop.asInchesString}" x2="${pts.xRight.asInchesString}" y2="${pts.yMiddle.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xRight.asInchesString}" y1="${pts.yMiddle.asInchesString}" x2="${pts.xMiddle.asInchesString}" y2="${pts.yBottom.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xMiddle.asInchesString}" y1="${pts.yBottom.asInchesString}" x2="${pts.xLeft.asInchesString}" y2="${pts.yMiddle.asInchesString}" style="$styleString" />\n""")
    sb.append(s"""<line x1="${pts.xLeft.asInchesString}" y1="${pts.yMiddle.asInchesString}" x2="${pts.xMiddle.asInchesString}" y2="${pts.yTop.asInchesString}" style="$styleString" />\n""")
  }
  private def drawEllipse(sb : StringBuilder, pts : BoxPoints, styleString : String) = {
    sb.append(s"""<ellipse cx="${pts.xMiddle.asInchesString}" cy="${pts.yMiddle.asInchesString}" rx="${pts.rx.asInchesString}" ry="${pts.ry.asInchesString}" style="$styleString" />\n""")
  }

}

case class PageFlow(contentID : String, color : String, location : Location) {
  private var blocks = new ListBuffer[PageBlock]
  private var paragraphLength = Length.dimension("0fu")

  def addBlock(value : PageBlock) : PageFlow = {
    blocks += value
    this
  }
  def displayString : String = {
    val sb = new StringBuilder(toString+"\n")
    blocks.foreach(b => sb.append("    "+b.displayString))
    sb.toString()
  }

  def toSVG(parentColor : String) : String = {
    val res = new StringBuilder(s"""<g id="$toString">\n""")
    if ((color != parentColor) && !((color == "#FFFFFF") && (parentColor == "white"))) {
      val x = location.left.asInchesString
      val y = location.top.asInchesString
      val w = location.width.asInchesString
      val h = location.height.asInchesString
      var colorString = color
      colorString match {
        case "" => colorString = "white"
      //  case "form-background" => colorString = "silver"
        case _ => parentColor
      }
      res.append(s"""<rect x="$x" y="$y" width="$w" height="$h" fill="$colorString" fill-opacity="0.6" id="$contentID" />\n""")
    }
    var loc = location.copyOf
    var bottom = loc.top
    blocks.foreach(b => if (!b.isEmpty)
      {
        res.append(b.toSVG(loc, paragraphs))
        loc = loc.moveDown(b.bottom - loc.top)
      })
    res.append("</g>\n")
    res.toString()
  }

  def paragraphs : Boolean = {
    if (paragraphLength.isEmpty) {
      blocks.foreach(b => paragraphLength = paragraphLength + Length.NEW_LINE_SIZE)
      paragraphLength += Length.NEW_LINE_SIZE
    }
    paragraphLength < location.height
  }
}

case class PageContent(title : String, pageMaster: PageMaster) {
  private var flows = new ListBuffer[PageFlow]

  def addFlow(value : PageFlow) : PageContent = {
     flows += value
     this
  }
  def displayString : String = {
    val sb = new StringBuilder(toString+"\n")
    flows.foreach(f => sb.append("  "+f.displayString))
    sb.toString()
  }

  def toSVG : String = {
    val width = pageMaster.width.asInchesString
    val height = pageMaster.height.asInchesString
    var color = pageMaster.color
    if (color == "")
      color = "white"
    color = "lightgreen"
    // dont really need a viewbox
    //    val res = new StringBuilder(s"""<svg width="$width" height="$height" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 708.000 139.200" >""")
    val res = new StringBuilder(s"""<svg width="$width" height="$height" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" id="$title" >\n""")
    res.append(s"""<rect x="0" y="0" width="$width" height="$height" fill="$color" fill-opacity="0.8" />\n""")

    flows.foreach(f => res.append(f.toSVG(color)))
    res.append("</svg>")
    res.toString()

  }
}

object PageContent {

  def requiredLength(numchars : Int, font : GidsFont) : Length = {
    Length.dimension((numchars * font.adjustedFontsize).toString+"pt")
  }

  def charsAvail(availLength : Length, font : GidsFont) : Int = {
    Length.dimension( (availLength.asPoints / font.adjustedFontsize ).toString+"pt").asPoints.toInt
  }

  def testLengths(charCount : Int, font : GidsFont) = {
    val text = "This is my First Test of text."
    val usedLength = font.stringWidth(text)
    val numUnits = usedLength.asDeviceUnits
    val availWidthUnits = Length.dimension("7.5in").asDeviceUnits
    println(" used:"+usedLength.asInchesString+s" chars:$charCount font:$font numChars:$numUnits availWidth:$availWidthUnits")
  }

}

