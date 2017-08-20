package com.westat.gids

import com.westat.Length

import scala.collection.mutable.ListBuffer
import java.awt.Font
import java.awt.image.BufferedImage


case class GidsFont(family : String, color : String, weightNum : String, rawSize : Length, isItalic : Boolean = false) {
  private lazy val javaFont = privFont

  def weight : String = {
    weightNum match {
      case "700" => "bold"
      case _ => "normal"
    }
  }
  def style : String = {
    isItalic match {
      case true => "italic"
      case false => "normal"
    }
  }
  def colorString : String = {
    if (color.length == 0)
      "black"
    else
      color
  }
  def sizeString : String = {
    rawSize.asPointsString
  }
  def adjustedFontsize : Double = {
    var adj = rawSize.asPoints
//    var delta = 0.548
    var delta = 0.546
    if (weight == "bold")
      delta = delta + 0.1
    if (style == "italic")
      delta = delta - 0.15
    (adj * delta)
  }
  def isEmpty : Boolean = family.isEmpty
  def notEqual(aFont : GidsFont) : Boolean = this.toString != aFont.toString
  def asSVGString : String = {
    if (family.isEmpty)
      ""
    else
      s"""fill="$colorString" font-family="$family" font-weight="$weight" font-style="$style" font-size="$sizeString" """
  }

  private def privFont : Font = {
    val fstyle = if (weight == "bold")
      Font.BOLD
    else
      if (style == "italic")
        Font.ITALIC
      else
        Font.PLAIN

    val intSize = if (rawSize == null)
      12
    else
      rawSize.asPoints.toInt

    val smallSize = if (rawSize == null)
      11
    else
      rawSize.asPoints.toInt

    new Font(family, fstyle, smallSize)
  }

  def stringWidth(text : String) : Length = {
   val graphics = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB).createGraphics
   val fontMetrics = graphics.getFontMetrics(javaFont)
   Length.dimension(s"${fontMetrics.stringWidth(text)}pt")
//    Length.dimension(s"${fontMetrics.stringWidth(text)+(adjustedFontsize*.75)}pt")
  }

  def lineSpace : Length = rawSize * 0.80
/*
  def lineHeight : Length = {
    if (fheight == null) {
      val graphics = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB).createGraphics
      val font = new Font(family, 0, rawSize.asPoints.toInt)
      val fontMetrics = graphics.getFontMetrics(font)
      fheight = Length.dimension(s"${fontMetrics.getHeight}pt")
  println(s"lineheight calced as ${fheight.asPoints} and raw was ${rawSize.asPoints}")
    }
    fheight
  }
  */
}

object GidsFont {
  val nullFont = GidsFont("", "", "", Length.dimension("10fu"))
  def fontFromStrings(ffamily : String, fcolor : String, fsize : String, fweight : String) : GidsFont = {
    if (ffamily.isEmpty)
      nullFont
    else
      GidsFont(ffamily, fcolor, fweight, Length.dimension(fsize))
  }
}


