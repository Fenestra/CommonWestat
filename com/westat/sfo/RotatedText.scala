package com.westat.sfo

import com.westat.gids.GidsFont
import com.westat.{Location, Length}

/**
  * Created by lee on 8/4/17.
  *
  * The problem with Rotated text is that current SVG specs require that all transforms use pixels as their units.
  * However, Linux reports hard-coded value of 90 for all devices, but Windows reports actual DPI, so you get
  * diff renderings on diff machines.  Therefore the SVG code is not portable.  There are 2 solutions:
  *
  * 1. turn the code requiring transforms into a mini self-contained SVG using its own UOM and hope that
  * it works on the transform, or simply scale it using the svg x,y width,height and viewbox to make it
  * appear as it is supposed to look.
  *
  * 2. place your own value in a properties file so that it can be set by the user.  They will have control over
  * how the rendering occurs - they can even emulate a different DPI in case further processing is done on a different
  * machine with diff resolution. For example, a machine that will turn the SVG into PDF may reside on only one of their
  * computers and that may have a very different DPI.
  *
  */


case class BlockRotatedText(data : String, angle : String, font : GidsFont, width : Length, height : Length) extends PageBlock {

  def bottom : Length = Length.dimension("0fu")

  def displayString : String = {
    toString+"\n"
  }

  def isEmpty : Boolean = data.length < 1

  def toSVG(location: Location, paragraphs: Boolean): String = {
   // <svg id="45rotated" x="1.57in" y="5.00in" width="1.0in" height="1in" viewBox="16 10 100 100">
    val sb = new StringBuilder(
      s"""<svg x="${location.left.asInchesString}" y="${location.top.asInchesString}" width="${location.width.asInchesString}" height="${location.height.asInchesString}">\n""")
      // <g transform="scale(0.8) rotate(45 50,50)"> to scale 80%
      //requested 1/2in ourscale = 1in so scale= 0.5 / ourscale   or  4 req / 1
      //val scale = 1 // requestedwidth / ourwidth
      // however, this means that we are scaling size of text, so it negates the user settings to do this!
      // if user needs diff size text, they should change the font size
    sb.append(s""" <g transform="rotate($angle 50,50)">\n""")
  //      <rect x="0" y="0" width="150" height="150" fill="pink"/>
  //      <text x="0" y="50" style="font-size:10pt;stroke:none;fill:black;font-weight:normal;font-family:Arial;text-anchor:start;">
  //        Some 45 rotated text
    val textPos = if (font.rawSize > Length.dimension("30pt"))
      """x="40" y="40" """
    else
      """x="0" y="50" """
    sb.append(
      s"""  <text $textPos style="text-anchor:start;" ${font.asSVGString}>$data</text>\n""")
    sb.append(" </g>\n</svg>")
    sb.toString()
  }

}

object RotatedText {
  def createRotatedText(compressedSource: String): BlockRotatedText = {
    val info = RotatedTextInfo(compressedSource)
    BlockRotatedText(info.text, info.angle, info.font, info.width, info.height)
  }
}

case class RotatedTextInfo(compressedSource : String) {
  var angleAsInt : Int = 0
  var text : String = _
  var fontName : String = _
  var color : Int = 0
  var fontSize : Int = 0
  var fontStyle : Int = 0
  var widthAsInt : Int = 0
  var heightAsInt : Int = 0
  var fontColorName : String = _
  def angle : String = "%.1f".format(angleAsInt / 10.0)
  def width : Length = Length.dimension(s"${widthAsInt}pt")
  def height : Length = Length.dimension(s"${heightAsInt}pt")
  expandAndReadData

  private def expandAndReadData : RotatedTextInfo = {
    val exp = ExpansionObj(compressedSource)
    angleAsInt = exp.readInt
    text = exp.readWideString
    fontName = exp.readString
    color = exp.readInt // -26 3 times
    fontSize = exp.readInt
    fontStyle = exp.readInt //45 is actually 5 long because its a set!
    exp.readByte
    heightAsInt = exp.readShortInt
    widthAsInt = exp.readShortInt
    fontColorName = exp.safeReadString //54
//    println(s"test is angle:$angle text:$text font:$fontname color:$color size:$size fs:$fontstyle w:$width h:$height fc:$fontcolorname")
    this
  }

  def font : GidsFont = {
    GidsFont(fontName, fontColorName, "", Length.dimension(s"${fontSize}pt"))
  }

}
/*---------------------------------------------------------------------

  if Source is TRotatedText then begin
    Rot := TRotatedText(Source);
    FAngle := Rot.Angle;
    FText := Rot.Text;
    FFont.Assign(Rot.Font);
    FHeight := Rot.Height;
    FWidth := Rot.Width end

  Stream.Read(FAngle, SizeOf(FAngle));
  Stream.Read(N, SizeOf(N));
  SetLength(FText, N);
  Stream.Read(FText[1], Length(FText) * SizeOf(WideChar));
  Stream.Read(N, SizeOf(N));
  SetLength(S, N);
  Stream.Read(S[1], Length(S));
  Font.Name := S;
  Stream.Read(N, SizeOf(N));
  Font.Color := N;
  Stream.Read(N, SizeOf(N));
  Font.Size := N;
  Stream.Read(FS, SizeOf(FS));
  Font.Style := FS;
  Stream.Read(FHeight, SizeOf(FHeight));
  Stream.Read(FWidth, SizeOf(FWidth));
  if Stream.Position < Stream.Size then begin
    Stream.Read(N, SizeOf(N));
    SetLength(FFontColorName, N);
    Stream.Read(FFontColorName[1], Length(FFontColorName));
    Font.Color := CColor.color(FFontColorName).asColor end end;

 */