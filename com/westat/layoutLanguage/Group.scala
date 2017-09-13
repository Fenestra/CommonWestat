package com.westat.layoutLanguage

import com.westat.{Length, Location}

import scala.collection.mutable.StringBuilder

/**
 * Created by lee on 6/30/17.
 */
object LayoutOrientations extends Enumeration {
  val loHorizontal = Value("horizontal")
  val loVertical   = Value("vertical")
  def valueForOrientationString(s : String) : LayoutOrientations.Value = {
    s match {
      case "horizontal" => loHorizontal
      case "vertical"   => loVertical
      case _            => loHorizontal
    }
  }
}

trait OrientedGroup extends Container {
  protected def orientation : LayoutOrientations.Value = LayoutOrientations.loHorizontal
  protected def location : Location = null
  protected def svgID : String = ""
  private var sectionIndex : Int = 0
  protected var secLoc : Location = null
  private var secSize : Length = Length.dimension("0fu")
  protected def startSections = {
    sectionIndex = 0
    // the new scheme is to setup the sectionLocation with the new sectionSize and new first location
    orientation match {
      case LayoutOrientations.loHorizontal => {
        secSize = location.width / children.size
        secLoc = secLoc.shrinkWidth(location.width - secSize)
      }
      case LayoutOrientations.loVertical   => {
        secSize = location.height / children.size
        secLoc = secLoc.shrinkHeight(location.height - secSize)
      }
    }
  }
  protected def nextSection : Location = {
    sectionIndex += 1
    // first section is already setup
    if (sectionIndex == 1)
      return secLoc
    orientation match {
      case LayoutOrientations.loHorizontal => secLoc = secLoc.moveRight(secSize)
      case LayoutOrientations.loVertical   => secLoc = secLoc.moveDown(secSize)
    }
    secLoc
  }
  override def toSVG : String = {
    val sb = new StringBuilder(s""" <g id="$svgID" >\n""")
    startSections
    children.foreach(c => {
      val loc = nextSection
      sb.append(c.toSvg(loc))
    })
    sb.append(" </g>\n")
    sb.toString
  }
}

case class Group(name : String, anOrientation : LayoutOrientations.Value, aLocation : Location, sectionSize : Length) extends OrientedGroup {
  override def orientation : LayoutOrientations.Value = anOrientation

  override def location: Location = aLocation
  secLoc = aLocation.copyOf
  override def svgID : String = s"$name-$orientation"
}

