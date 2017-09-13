package com.westat.sfo

import com.westat.gids.GidsFont
import com.westat.{Location, Length}

/**
  * Created by lee on 8/4/17.
  */

object ReverseCircleKinds extends Enumeration {
  val rckEcon       = Value("econ")
  val rckACSBlack   = Value("acs-black")
  val rckACSArrow   = Value("acs-arrow")
  val rckACSColor   = Value("acs-color")
  val rckDecennial  = Value("decennial")
  def valueForKindString(s : String) : ReverseCircleKinds.Value = {
    s match {
      case "econ"       => rckEcon
      case "acs-black"  => rckACSBlack
      case "acs-arrow"  => rckACSArrow
      case "acs-color"  => rckACSColor
      case "decennial"  => rckDecennial
      case _            => rckEcon
    }
  }
}

case class RCStyle(circleKind : ReverseCircleKinds.Value) {
  def color : String = {
    circleKind match {
      case ReverseCircleKinds.rckACSColor  => "grey" //acs-reverse-circle
      case _            => "black"
    }
  }

  def fontFamily : String = {
    circleKind match {
      case ReverseCircleKinds.rckACSArrow  => "DejaVu Sans"
      case _            => "Helvetica" // "Univers LT 55"
    }
  }

  def fontSize : String = {
    circleKind match {
      case ReverseCircleKinds.rckEcon      => "9.0pt"
      case ReverseCircleKinds.rckACSBlack  => "10.0pt"
      case ReverseCircleKinds.rckACSArrow  => "17.0pt"
      case ReverseCircleKinds.rckACSColor  => "13.0pt"
      case ReverseCircleKinds.rckDecennial => "10.0pt"
    }
  }

  def fontWeight : String = "700"

  def fontColor : String = {
    circleKind match {
      case ReverseCircleKinds.rckACSColor  => "black"
      case _            => "white"
    }
  }

  def radius : String = {
    circleKind match {
      case ReverseCircleKinds.rckEcon      => "0.100in"
      case _            => "0.116in"
    }
  }

  def attributes : String = {
    s"$toString col:$color fam:$fontFamily sz:$fontSize wgt:$fontWeight fcol:$fontColor rad:$radius"
  }

  def fontAttributes : String = {
    s"""stroke="$color" font-size="$fontSize" fill="$fontColor" font-family="$fontFamily" font-weight="bold" text-anchor="middle" """
  }

}

case class BlockReverseCircle(circleKind : ReverseCircleKinds.Value, content : String) extends PageBlock {
  private val style = RCStyle(circleKind)

  def bottom: Length = Length.dimension("0fu")

  def displayString: String = {
    toString + "\n"
  }

  def isEmpty: Boolean = false

  def toSVG(location: Location, paragraphs: Boolean): String = {
    val sb = new StringBuilder()
    val width = Length.dimension(".4in")
    val height = Length.dimension(".4in")
    val midLeft = location.left + (width / 2)
    val midTop = location.top + (height / 2)
    //println(s"here we have the style ${style.attributes}")
    sb.append(s"""<circle cx="${midLeft.asInchesString}" cy="${midTop.asInchesString}" r="${style.radius}" fill="${style.color}" />\n""")
    sb.append(
      s"""<text x="${midLeft.asInchesString}" y="${(midTop + (Length.dimension(style.radius) / 2)).asInchesString}" ${style.fontAttributes}>$content</text>\n""")
    sb.toString()
  }

}

case class InlineReverseCircle(circleKind : ReverseCircleKinds.Value, content : String, font : GidsFont) extends TextObject {
  private val style = RCStyle(circleKind)

  def bottom: Length = Length.dimension("0fu")

  def displayString: String = {
    toString + "\n"
  }

  def isEmpty: Boolean = false

  def fontstring : String = {
    if (font != null)
      font.asSVGString
    else
      style.fontAttributes
  }

  def toSVG(location: Location, paragraphs: Boolean): String = {
    val sb = new StringBuilder()
    val width = Length.dimension(".4in")
    val height = Length.dimension(".4in")
    val midLeft = location.left + (width / 2)
    val midTop = location.top + (height / 2)
    //println(s"here we have the style ${style.attributes}")
    sb.append(s"""<circle cx="${midLeft.asInchesString}" cy="${midTop.asInchesString}" r="${style.radius}" fill="${style.color}" />\n""")
    sb.append(
      s"""<text x="${midLeft.asInchesString}" y="${(midTop + (Length.dimension(style.radius) / 2)).asInchesString}" ${fontstring}>$content</text>\n""")
    sb.toString()
  }

}

  /*

COLORS: array[TReverseCircleStyle, TReverseCircleContext] of WideString =
( ( 'black'               // rcsEcon        / block
, 'black'               // rcsEcon        / inline
)
, ( 'black'               // rcsACSBlack    / block
, 'black'               // rcsACSBlack    / inline
)
, ( 'black'               // rcsACSArrow    / block
, 'black'               // rcsACSArrow    / inline
)
, ( 'acs-reverse-circle'  // rcsACSColor    / block
, 'acs-reverse-circle'  // rcsACSColor    / inline
)
, ( 'black'               // rcsDecennial   / block    !! placeholder
, 'black'               // rcsDecennial   / inline   !! placeholder
)
, ( 'black'               // rcsUnspecified / block
, 'black'               // rcsUnspecified / inline
)
);

FONT_FAMILYS: array[TReverseCircleStyle, TReverseCircleContext] of WideString =
( ( 'Univers LT 55'  // rcsEcon        / block
, 'Univers LT 55'  // rcsEcon        / inline
)
, ( 'Univers LT 55'  // rcsACSBlack    / block
, 'Univers LT 55'  // rcsACSBlack    / inline
)
, ( 'DejaVu Sans'    // rcsACSArrow    / block
, 'DejaVu Sans'    // rcsACSArrow    / inline
)
, ( 'Univers LT 55'  // rcsACSColor    / block
, 'Univers LT 55'  // rcsACSColor    / inline
)
, ( 'Univers LT 55'  // rcsDecennial   / block    !! placeholder
, 'Univers LT 55'  // rcsDecennial   / inline   !! placeholder
)
, ( 'Univers LT 55'  // rcsUnspecified / block
, 'Univers LT 55'  // rcsUnspecified / inline
)
);

FONT_SIZES: array[TReverseCircleStyle, TReverseCircleContext] of WideString =
( (  '9.0pt'  // rcsEcon        / block
,  '6.3pt'  // rcsEcon        / inline
)
, ( '10.0pt'  // rcsACSBlack    / block
,  '7.9pt'  // rcsACSBlack    / inline
)
, ( '17.0pt'  // rcsACSArrow    / block
, '13.0pt'  // rcsACSArrow    / inline
)
, ( '13.0pt'  // rcsACSColor    / block
, '10.3pt'  // rcsACSColor    / inline
)
, ( '10.0pt'  // rcsDecennial   / block    !! placeholder
,  '7.9pt'  // rcsDecennial   / inline   !! placeholder
)
, (  '9.0pt'  // rcsUnspecified / block
,  '6.3pt'  // rcsUnspecified / inline
)
);

FONT_WEIGHTS: array[TReverseCircleStyle, TReverseCircleContext] of Integer =
( ( 700  // rcsEcon        / block
, 700  // rcsEcon        / inline
)
, ( 700  // rcsACSBlack    / block
, 700  // rcsACSBlack    / inline
)
, ( 700  // rcsACSArrow    / block
, 700  // rcsACSArrow    / inline
)
, ( 700  // rcsACSColor    / block
, 700  // rcsACSColor    / inline
)
, ( 700  // rcsDecennial   / block    !! placeholder
, 700  // rcsDecennial   / inline   !! placeholder
)
, ( 700  // rcsUnspecified / block
, 700  // rcsUnspecified / inline
)
);

FONT_COLORS: array[TReverseCircleStyle, TReverseCircleContext] of WideString =
( ( 'white'  // rcsEcon        / block
, 'white'  // rcsEcon        / inline
)
, ( 'white'  // rcsACSBlack    / block
, 'white'  // rcsACSBlack    / inline
)
, ( 'white'  // rcsACSArrow    / block
, 'white'  // rcsACSArrow    / inline
)
, ( 'black'  // rcsACSColor    / block
, 'black'  // rcsACSColor    / inline
)
, ( 'white'  // rcsDecennial   / block    !! placeholder
, 'white'  // rcsDecennial   / inline   !! placeholder
)
, ( 'white'  // rcsUnspecified / block
, 'white'  // rcsUnspecified / inline
)
);

RADIUSS: array[TReverseCircleStyle, TReverseCircleContext] of WideString =
( ( '0.100in'  // rcsEcon        / block
, '0.062in'  // rcsEcon        / inline
)
, ( '0.116in'  // rcsACSBlack    / block
, '0.093in'  // rcsACSBlack    / inline
)
, ( '0.116in'  // rcsACSArrow    / block
, '0.093in'  // rcsACSArrow    / inline
)
, ( '0.116in'  // rcsACSColor    / block
, '0.093in'  // rcsACSColor    / inline
)
, ( '0.116in'  // rcsDecennial   / block    !! placeholder
, '0.093in'  // rcsDecennial   / inline   !! placeholder
)
, ( '0.100in'  // rcsUnspecified / block
, '0.062in'  // rcsUnspecified / inline
)
);

*/


