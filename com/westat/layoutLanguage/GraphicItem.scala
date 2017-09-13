package com.westat.layoutLanguage

import com.westat.{Length, Location}
import com.westat.gids.GidsFont
import com.westat.sfo._
import scala.collection.mutable.StringBuilder

/**
 * Created by lee on 6/30/17.
 */

trait GraphicItem {
  protected val defaultWidth = Length.dimension("1in")
  protected val defaultHeight = Length.dimension("1in")
  protected val defaultSpace = Length.dimension("0fu")
  def toSvg(loc : Location) : String = toString+"\n"
}

trait TextItem extends GraphicItem {
  def addText(text: String, aFont: GidsFont = null): TextItem
}

case class GraphicImage(data : String) extends GraphicItem

case class GraphicShape(graphicType : String) extends GraphicItem {
  override def toSvg(loc : Location) : String = {
    val sb = new StringBuilder(s"   <text>image:$graphicType</text>\n")
    sb.toString()
  }
}

case class GraphicText(font : GidsFont, color : String, textAlign : TextAlignments.Value, text : String) extends TextItem {
  private val blocktext = BlockText(font, textAlign)
  blocktext.addText(InlineText(text, font))

  override def toSvg(loc : Location) : String = {
    blocktext.toSVG(loc, false)
  }

  def addText(text : String, aFont : GidsFont = null) : TextItem = {
    if (aFont == null)
      blocktext.addText(InlineText(text, font))
    else
      blocktext.addText(InlineText(text, aFont))
    this
  }
}

trait GraphicWithCaption extends GraphicItem {
  def graphicSVG(loc : Location) : String
  def captionText : String
  override def toSvg(loc : Location) : String = {
    val sb = new StringBuilder(graphicSVG(loc))
    val cent = loc.left + (defaultWidth / 2)
    sb.append(s"""   <text x="${cent.asInchesString}" y="${(loc.top + defaultHeight + Length.dimension("9pt")).asInchesString}" text-anchor="middle" >$captionText</text>\n""")
    sb.toString()
  }
}

trait TextWithCaption extends GraphicWithCaption {
  def addText(text: String, aFont: GidsFont = null): TextWithCaption
}

case class GraphicImageWithCaption(data : String, text : String) extends GraphicWithCaption {
  private val image = BlockGraphic.createGraphic("png", Length.dimension(".75in"), Length.dimension(".75in"),
    Length.dimension("0fu"), Length.dimension("0fu"), ImageData.SEAL)

  override def graphicSVG(loc : Location) : String = {
    image.toSVG(loc, false)
  }

  override def captionText : String = text
}

case class GraphicBarcodeWithCaption(data : String, text : String) extends GraphicWithCaption {
  private val image = BlockGraphic.createGraphic("bar-code", Length.dimension("1in"), Length.dimension(".5in"),
    Length.dimension("0fu"), Length.dimension("0fu"), data)

  override def graphicSVG(loc : Location) : String = {
    image.toSVG(loc, false)
  }

  override def captionText : String = text
}

case class GraphicEyeReadableWithCaption(data : String, text : String) extends GraphicWithCaption {
  private val image = BlockGraphic.createGraphic("eye-readable-number", Length.dimension("1in"), Length.dimension(".5in"),
    Length.dimension("0fu"), Length.dimension("0fu"), data)

  override def graphicSVG(loc : Location) : String = {
    image.toSVG(loc, false)
  }

  override def captionText : String = text
}

case class GraphicRotatedTextWithCaption(data : String, angle : String, font : GidsFont, text : String) extends GraphicWithCaption {
  private val rot = BlockRotatedText(data, angle, font, Length.dimension("1in"), Length.dimension("1in"))

  override def graphicSVG(loc : Location) : String = {
    val sloc = loc.adjustPadding(Length.dimension("1pt"), Length.dimension("1pt"))
    s"""<rect x="${sloc.left.asInchesString}" y="${sloc.top.asInchesString}" width="${(sloc.right - sloc.left).asInchesString}" height="${(sloc.bottom - sloc.top).asInchesString}" fill="pink"/>\n """ +
      rot.toSVG(loc, false)
  }

  override def captionText : String = text
}

case class GraphicReverseCircleWithCaption(circleKind : ReverseCircleKinds.Value, content : String, text : String) extends GraphicWithCaption {
  private val rot = BlockReverseCircle(circleKind, content)

  override def graphicSVG(loc : Location) : String = {
    s"""<rect x="${loc.left.asInchesString}" y="${loc.top.asInchesString}" width="${(loc.right - loc.left).asInchesString}" height="${(loc.bottom - loc.top).asInchesString}" fill="pink"/>\n """ +
      rot.toSVG(loc, false)
  }

  override def captionText : String = text
}

case class GraphicBracketWithCaption(direction : String, text : String, color : String) extends GraphicWithCaption {
  private val bracket = BlockBracket(direction, Length.dimension("0fu"), color, null)

  override def graphicSVG(loc : Location) : String = {
    bracket.toSVG(loc, false)
  }

  override def captionText : String = text
}

case class GraphicShapeWithCaption(graphicType : String, text : String, width : Length = Length.dimension("0fu"), height : Length = Length.dimension("0fu")) extends GraphicWithCaption {
  private def graphicWidth : Length = if (width.equals("0fu"))
    defaultWidth
  else
    width
  private def graphicHeight : Length = if (height.equals("0fu"))
    defaultHeight
  else
    height
  private val graphicShape = BlockBox(BoxStyles.valueForStyleString(graphicType), graphicWidth, graphicHeight,
    defaultSpace, defaultSpace, Length.dimension(".01in"), "blue", "blue")

  override def graphicSVG(loc : Location) : String = {
    graphicShape.toSVG(loc, false)
  }

  override def captionText : String = text
}

case class GraphicTextWithCaption(font : GidsFont, color : String, textAlign : TextAlignments.Value, text : String, caption : String) extends TextWithCaption {
  private val blocktext = BlockText(font, textAlign)
  blocktext.addText(InlineText(text, font))

  override def graphicSVG(loc : Location) : String = {
    s"""<rect x="${loc.left.asInchesString}" y="${loc.top.asInchesString}" width="${(loc.right - loc.left).asInchesString}" height="${(loc.bottom - loc.top).asInchesString}" fill="$color"/>\n """ +
      blocktext.toSVG(loc, false)
  }

  override def captionText : String = caption

  def addText(text : String, aFont : GidsFont = null) : TextWithCaption = {
    if (aFont == null)
      blocktext.addText(InlineText(text, font))
    else
      blocktext.addText(InlineText(text, aFont))
    this
  }
}

