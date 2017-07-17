package com.westat

case class LayoutPoint(left : Length, top : Length)

case class BoxPoints(xLeft : Length, xMiddle : Length, xRight : Length, yTop : Length,
                     yMiddle : Length, yBottom : Length, rx : Length, ry : Length)

trait Location {
  def left : Length
  def top : Length
  def right : Length
  def bottom : Length
  def width : Length
  def height : Length
  def rectString : String
  def copyOf : Location
  def moveLeft(delta : Length) : Location
  def moveRight(delta : Length) : Location
  def moveUp(delta : Length) : Location
  def moveDown(delta : Length) : Location
  // shifts area over by the amount that was used == available remaining area
  def shrinkWidth(delta : Length) : Location
  def shrinkHeight(delta : Length) : Location
  def isEmpty : Boolean = {
    left + top + right + bottom + width + height == Length.dimension("0fu")
  }
  def adjustPadding(leftPad : Length, rightPad : Length) : Location = {
    var newLeft = left
    var newRight = right
    if (leftPad != null)
      newLeft   = newLeft + leftPad
    if (rightPad != null)
      newRight  = newRight - rightPad
    Location.create(newLeft, top, newRight - newLeft, height)
  }
}

object Location {
  def create(left : Length, top : Length, width : Length, height : Length) : Location = {
    NewLocation(left, top, width, height)
  }

  def create(leftStr : String, topStr : String, widthStr : String, heightStr : String) : Location = {
    NewLocation(Length.dimension(leftStr), Length.dimension(topStr), Length.dimension(widthStr), Length.dimension(heightStr))
  }
}

case class NewLocation(left : Length, top : Length, width : Length, height : Length) extends Location {
  def right : Length = left + width
  def bottom : Length = top + height
  def rectString : String = s"rect (${left.asInchesString}, ${top.asInchesString})  (${right.asInchesString}, ${bottom.asInchesString})  (${width.asInchesString}, ${height.asInchesString})"
  def copyOf : Location = NewLocation(left, top, width, height)
  def moveLeft(delta : Length) : Location = NewLocation(left - delta, top, width, height)
  def moveRight(delta : Length) : Location = NewLocation(left + delta, top, width, height)
  def moveUp(delta : Length) : Location = NewLocation(left, top - delta, width, height)
  def moveDown(delta : Length) : Location = NewLocation(left, top + delta, width, height)
  def shrinkWidth(delta : Length) : Location = NewLocation(left, top, width - delta, height)
  def shrinkHeight(delta : Length) : Location = NewLocation(left, top, width, height - delta)
  override def toString(): String = {
    "Location(" + rectString + ")"
  }
}

