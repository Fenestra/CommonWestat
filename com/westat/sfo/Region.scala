package com.westat.sfo

import com.westat.gids.GidsFont
import com.westat.{Location, Length, StringUtilities}
import scala.collection.mutable.ListBuffer



trait QnrItem {
  def id : String
  def x : Length
  def y : Length
  def name : String
  def font : GidsFont
  def toSVG(maxWidth : Length) : String = {
    if (name == "?")
      return ""
    val left = x.asInchesString
    val top = y.asInchesString
    val fontStr = font.asSVGString
    var charCount = maxWidth.asPoints / 5
    if (!font.isEmpty)
       charCount = maxWidth.asPoints / (font.rawSize.asPoints / 2)
    val sb = new StringBuilder(s"""<text x="$left" y="$top" $fontStr>""")
    val lines = StringUtilities.listOfMaxLengthStrings(name, charCount.toInt)
    if (lines.length == 1)
      sb.append(name)
    else
      lines.foreach(s => sb.append(s"""<tspan x="$left" dy="15" >"""+s+"</tspan>\n"))
    sb.append("</text>\n")
    sb.toString()
  }
}

case class QItem(anID : String, aX : Length, aY : Length, aName : String, aFont : GidsFont) extends QnrItem {
  def id : String = anID
  def x : Length = aX
  def y : Length = aY
  def name : String = aName
  def font : GidsFont = aFont
}

case class QCheckboxItem(anID : String, aX : Length, aY : Length, deName : String, aFont : GidsFont) extends QnrItem {
  def id : String = anID
  def x : Length = aX
  def y : Length = aY
  def name : String = deName
  def font : GidsFont = aFont
}

case class QAnswerboxItem(anID : String, aX : Length, aY : Length, deName : String, aFont : GidsFont) extends QnrItem {
  def id : String = anID
  def x : Length = aX
  def y : Length = aY
  def name : String = deName
  def font : GidsFont = aFont
}

case class QGraphic(anID : String, aX : Length, aY : Length, aClass : String,
                    width : Length, height : Length, spaceBefore : Length, spaceAfter : Length, somedata : String)
                    extends QnrItem {
  def id : String = anID
  def x : Length = aX
  def y : Length = aY
  def name : String = aClass
  def font : GidsFont = GidsFont.nullFont
 //eJw0WgVQW12wDlCgxYu7FCgOpbi7uwR3KR4cilvRQHEoLkGDU4JbW5wCxV2KO
  def data : String = "eJw0WgVQW12wDlCgxYu7FCgOpbi7uwR3KR4cilvRQHEoLkGDU4JbW5wCxV2KO"
  override def toSVG(maxWidth : Length) : String = {
    val sb = new StringBuilder(s"""<image id="$id" x="${x.asInchesString}" y="${y.asInchesString}" """)
    sb.append(s"""width="${width.asInchesString}" height="${height.asInchesString}" """)
    sb.append(s"""xlink:href="data:image/jpg;base64,$data" />""")
    sb.toString()
  }
}

case class PageMaster(id : String, color : String, width : Length, height : Length) {
  var regions : List[Region] = null
  var contentItems : List[QnrItem] = null
  def findLocation(name : String) : Location = {
    regions.foreach(r => r.findLocation(name) match {
        case Some(loc) => return loc
        case None =>
      })
    location
  }

  def location : Location = {
    Location.create(Length.dimension("0fu"), Length.dimension("0fu"), width, height)
  }

  def setContentItems(aList : List[QnrItem]) = {
    contentItems = aList
  }

  def computeDimensions = {
    regions.foreach(r =>
       r.computeDimensions(this)
    )
  }

  def displayRegions = {
    regions.sortBy(r => r.top.asInches).foreach(r => r.displayAreaNames)
  }
}


case class Area(stacking : String, left : Length, top : Length, width : Length, height : Length, name : String) {
  val origLocation : Location = Location.create(
    Length.percentOrDim(left, Length.dimension("0fu")),
    Length.percentOrDim(top, Length.dimension("0fu")),
    Length.percentOrDim(width, Length.dimension("0fu")),
    Length.percentOrDim(height, Length.dimension("0fu")))
  var location = origLocation
  var areas : List[Area] = null

  def displayAreaNames(indent : String = "") : Unit = {
    if (name.nonEmpty) {
      println(indent+s"area: $name  $left $top")
    }
    if (areas.ne(null))
       areas.sortBy(a => a.top.asInches).foreach(a => a.displayAreaNames(indent+" "))
  }

  def area : Area = {
    areas.head
  }
  def remainingAreas : List[Area] = {
    areas.tail
  }

  def findLocation(name : String) : Option[Location] = {
    if (this.name == name)
      return Some(location)
    if (areas.ne(null))
      areas.foreach(a => a.findLocation(name) match {
      case Some(loc) => return Some(loc)
      case None =>
    })
    None
  }

  def spacerString(last : Int, current : Int) : String = {
    var s = ""
    if (last < current)
      for(i <- last to current) {
        s = s + " "
      }
    s
  }

  def dumpLocations(depth : Int = 1) : Area = {
    val sp = spacerString(0, depth)
    println(s" $sp area $name "+location.rectString)
    if (areas != null)
      areas.foreach(a => a.dumpLocations(depth+2))
    this
  }

  def childList : String = {
    var s = "    "+toString+"\n"
    if (areas != null)
       areas.foreach(a => s = s + "      "+a+"\n")
    s
  }

  def computeDimensions(par : Location) : Location = {
    location = Location.create(origLocation.left, origLocation.top, Length.percentOrDim(width, par.width), Length.percentOrDim(height, par.height))
    stacking match {
      case "top"    => // nothing to do, normal location.bottom = location.top + location.height
        location = if (location.left < par.left)
          location.moveRight(par.left - location.left)
        else
          location.moveLeft(location.left - par.left)
      case "bottom" => computeBottomStacking(par)
      case "left"   => // nothing to do, normal location.right = location.left + location.width
      case "right"  => computeRightStacking(par)
    }
//    val rect = computeChildDimensions(par)
    val rect = computeChildDimensions(location)
    location = rect
    location
  }

  def computeBottomStacking(par : Location) = {
    val specifiedHeight = Length.percentOrDim(height, par.height)
    location = Location.create(par.left, par.bottom - specifiedHeight, par.width, specifiedHeight)
  }

  def computeRightStacking(par : Location) = {
    val specifiedWidth = Length.percentOrDim(width, par.width)
    location = Location.create(par.right - specifiedWidth, par.top, specifiedWidth, par.height)
  }

  private def computeChildDimensions(last : Location) : Location = {
    if (areas == null)
       return location
    var loc = last
    areas.foreach(a => {
      loc = a.computeDimensions(location)
   //   loc = a.computeChildDimension(this)
    })
//    loc
    last
  }

  def computeChildDimension(par : Area) : Location = {
    location = par.location.copyOf
    location
  }

}


case class Region(left : Length, top : Length, right : Length, bottom : Length, 
                  width : Length, height : Length, transform : String, isBackground : String) {
  var location = Location.create(left, top, width, height)
  var areas : List[Area] = null

  def displayAreaNames : Unit = {
    areas.foreach(a => a.displayAreaNames())
  }

  def findLocation(name : String) : Option[Location] = {
    areas.foreach(a => a.findLocation(name) match {
      case Some(loc) => {
        // make copy of region, set left,top into it
        // adjust width and height left-reg.left sub from width  top-reg.top from height
        val aloc = Location.create(loc.left, loc.top, loc.width - (left-loc.left), loc.height - (top-loc.top))
        return Some(aloc)
      }
      case None =>
    })
    None
  }

  // computedimens, then make first area the Area,
  //   all the others are RemainingArea
  //   make list of areas, first one is head, remaining is tail
  def computeDimensions(pm : PageMaster) = {
    if (pm.ne(null)) {
      computeHeights(pm)
      computeWidths(pm)
    }
    computeAreaDimensions
  }

  private def computeHeights(pm : PageMaster) = {
    var specHeight : Length = null
    specHeight = Length.percentOrDim(height, pm.height)
    if (specHeight != null) 
      computeTopAndBottom(pm, specHeight)
    else 
      computeFromTop(pm)
  }

  private def computeFromTop(pm : PageMaster) = {
    var atop = Length.dimension("0fu")
    var abottom = Length.dimension("0fu")
    val specTop = Length.percentOrDim(top, pm.height)
    if (specTop != null)
      atop = Length.min(pm.height, specTop)
    val specBottom = Length.percentOrDim(bottom, pm.height)
    if (specBottom != null)
      abottom = pm.height - Length.min(pm.height - atop, specBottom)
    location = Location.create(location.left, atop, location.width, abottom - atop)
  }
  
  private def computeTopAndBottom(pm : PageMaster, specHeight : Length) = {
    val aheight = Length.min(pm.height, specHeight)
    location = Location.create(location.left, Length.min(pm.height - aheight, top), location.width, aheight)
  }
  
  private def computeWidths(pm : PageMaster) = {
    val specWidth = Length.percentOrDim(width, pm.width)
    if (specWidth != null) {
      val awidth = Length.min(pm.width, specWidth)
      location = Location.create(Length.min(pm.width - awidth, left), location.top, awidth, location.height)
    }
    else 
      computeFromTop(pm)
  }

  def computeAreaDimensions = {
    if (areas.ne(null)) {
      var loc = location.shrinkHeight(location.height)
      areas.foreach(a => {
        a.computeDimensions(loc)
        loc = a.location
      })
    }
  }

}

