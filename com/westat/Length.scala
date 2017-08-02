package com.westat

/**
 * Created by Lee on 11/17/2016.
 */

trait CodeType {
  def displayValue : String
}

trait UOM extends CodeType {
  def unitRatio : Int
}
/*TMetre         = Double;
TInch          = Double;
TCentimetre    = Double;
TPica          = Double;
TMillimetre    = Double;
TBigPoint      = Double;
TPrintersPoint = Double;
TFut           = Currency;
TPercent       = Double;
  UnitNames: array[TUnit] of WideString =('m', 'in', 'cm', 'pc', 'mm', 'pt', 'pp', 'fu');

  UnitRatios: array[TUnit] of Integer = (
    289080000,
      7342632,
      2890800,
      1219200,
       289080,
       101981,
       101600,
            1);
*/

case object Metre extends UOM {
  def displayValue : String = { "m" }
  def unitRatio : Int = { 289080000 }
}
case object Inch extends UOM {
  def displayValue : String = { "in" }
  def unitRatio : Int = { 7342632 }
}
case object Centimetre extends UOM {
  def displayValue : String = { "cm" }
  def unitRatio : Int = { 2890800 }
}
case object Pica extends UOM {
  def displayValue : String = { "pc" }
  def unitRatio : Int = { 1219200 }
}
case object Millimetre extends UOM {
  def displayValue : String = { "mm" }
  def unitRatio : Int = { 289080 }
}
case object Point extends UOM {
  def displayValue: String = { "pt" }
  def unitRatio: Int = { 101981 }
}
case object PrintersPoint extends UOM {
  def displayValue : String = { "pp" }
  def unitRatio : Int = { 101600 }
}
case object Fut extends UOM {
  def displayValue : String = { "fu" }
  def unitRatio : Int = { 1 }
}

/*
  Fut is base underlying unit that everything gets converted to (device units)
  class makes an object of specified uom and value
  object makes conversions and provides conversion
    primarily used to convert strings to values ie: Length.dimension("0.14in") to a double
    can then do calcs with the double and then ask for
 */
case class Length(uom : UOM, value : Double) {
  private val LDPI = 7342632

  def asFut : Double = {
     value * uom.unitRatio
  }
  def asPixels : Long = {
    val s : Double = asDeviceUnits * 0.9
    s.toLong
  }
  def asDeviceUnits : Long = {
    //    println(s"asDeviceUnits for $value asFut is "+asFut.toLong)
    val s : Double = (asFut / LDPI) * 100
    s.toLong
  }
  def fromDeviceUnits(newvalue : Long) : Length = {
    Length(Fut, (newvalue.toDouble / 100) * LDPI)
  }
  def equals(v : String) : Boolean = {
    asInchesString == Length.dimension(v).asInchesString
  }
  def +(v : Length) : Length = {
    Length(Fut, asFut + v.asFut)
  }
  def -(v : Length) : Length = {
    Length(Fut, asFut - v.asFut)
  }
  def *(v : Double) : Length = {
    Length(Fut, asFut * v)
  }
  def /(v : Double) : Length = {
    Length(Fut, asFut / v)
  }
  def >(v : Length) : Boolean = {
    asDeviceUnits > v.asDeviceUnits
  }
  def <(v : Length) : Boolean = {
    asDeviceUnits < v.asDeviceUnits
  }
  def >=(v : Length) : Boolean = {
    asDeviceUnits >= v.asDeviceUnits
  }
  def <=(v : Length) : Boolean = {
    asDeviceUnits <= v.asDeviceUnits
  }
  def asMetres : Double = {
    asFut / Metre.unitRatio
  }
  def asInches : Double = {
    asFut / Inch.unitRatio
  }
  def asCentimetres : Double = {
    asFut / Centimetre.unitRatio
  }
  def asPicas : Double = {
    asFut / Pica.unitRatio
  }
  def asMillimetres : Double = {
    asFut / Millimetre.unitRatio
  }
  def asPoints : Double = {
    asFut / Point.unitRatio
  }
  def asPrintersPoints : Double = {
    asFut / PrintersPoint.unitRatio
  }
  def asInchesString : String = {
    f"$asInches%1.2fin"
  }
  def asPointsString : String = {
    asPoints + "pt"
  }
  def translateUOMs : String = {
    val mm = asMillimetres
    val cm = asCentimetres
    val m = asMetres
    val in = asInches
    val pc = asPicas
    val pt = asPoints
    val pp = asPrintersPoints
    s"$this as mm:$mm cm:$cm m:$m in:$in pc:$pc pt:$pt pp:$pp dev:$asDeviceUnits px:$asPixels"
  }
  override def toString : String = {
    "Length("+asInchesString+")"
  }
  def isEmpty : Boolean = value.toInt == 0
}

// primarily serves to convert strings to Double
object Length {
  val NEW_LINE_SIZE = Length.dimension("0.15in")

  def test = {
    var d = dimension("25.4mm")
    println(s"dimension of 25.4mm is $d and deviceUnits is "+d.asDeviceUnits)
    d = dimension("2.54cm")
    println(s"dimension of 2.54cm is $d and deviceUnits is "+d.asDeviceUnits)
    d = dimension("1in")
    println(s"dimension of 1.in is $d and deviceUnits is "+d.asDeviceUnits)
    println("  and as other uoms is "+d.translateUOMs)
    d = dimension("2in")
    println(s"dimension of 2.in is $d and deviceUnits is "+d.asDeviceUnits)
    d = fromDeviceUnits(d.asDeviceUnits)
    println(s"dimension of 2in translated from devunits is $d and deviceUnits is "+d.asDeviceUnits)
    val du = d.asDeviceUnits
    d = dimension(".5in")
    d = fromDeviceUnits(du - d.asDeviceUnits)
    println(s"dimension of 2in less half inch translated from devunits is $d and deviceUnits is "+d.asDeviceUnits)
    d = dimension("1.275pc")
    println(s"dimension of 1.275pc is $d and deviceUnits is "+d.asDeviceUnits)
    d = dimension("22027896fu")
    println(s"dimension of 22027896fu is $d and deviceUnits is "+d.asDeviceUnits)
    d = dimension("3.in")
    println(s"move to 3.in is $d and deviceUnits is "+d.asDeviceUnits)
    d = add(d, ".5in")
    println(s"add .5in is $d and deviceUnits is "+d.asDeviceUnits)
    println("  and as other uoms is "+d.translateUOMs)
    d = sub(d, ".5in")
    println(s"sub .5in is $d and deviceUnits is "+d.asDeviceUnits)
    println("  and as other uoms is "+d.translateUOMs)
    d = dimension("1in")
    println(s"sub 1in is $d and deviceUnits is "+d.asDeviceUnits)
    println("  and as other uoms is "+d.translateUOMs)
    d = dimension("2in")
    println(s"sub 2in is $d and deviceUnits is "+d.asDeviceUnits)
    println("  and as other uoms is "+d.translateUOMs)
  }

  private def uomFromString(value : String) : UOM = {
//    y[TUnit] of WideString =('m', 'in', 'cm', 'pc', 'mm', 'pt', 'pp', 'fu');
     val re = "in$|cm$|pc$|mm$|pt$|pp$|fu$|m$".r
     var result : String = ""
     re.findFirstIn(value) match {
      case Some(um) => result = um
      case None =>
     }
     result match {
       case "m"  => Metre
       case "in" => Inch
       case "cm" => Centimetre
       case "pc" => Pica
       case "mm" => Millimetre
       case "pt" => Point
       case "pp" => PrintersPoint
       case "fu" => Fut
       case "" => Inch
     }
  }

  def dimension(value : String) : Length = {
    if (value.isEmpty)
      return null
    val uom = uomFromString(value);
    Length(uom, value.dropRight(uom.displayValue.length).toDouble)
  }

  def percentOrDim(value : String, ref : Length) : Length = {
    if (value.contains('%'))
      println("percentOrDim has a percentage")
    dimension(value)
  } 
  
  def percentOrDim(a : Length, b : Length) : Length = {
    if (a == null)
      b
    else
      a
  }

  def max(a : Length, b : Length) : Length = {
    if (a.asDeviceUnits > b.asDeviceUnits)
      a
    else
      b
  }

  def min(a : Length, b : Length) : Length = {
     if (a.asDeviceUnits < b.asDeviceUnits)
       a
     else
       b
  } 
  
  def fromDeviceUnits(newvalue : Long) : Length = {
    Length(Fut, (newvalue.toDouble / 100) * 7342632)
  }


  // need to be able to do math via common Fut, but displayString in any uom!
  def add(d : Length, value : String) : Length = {
    val uom = uomFromString(value);
    val amt = Length(uom, value.dropRight(uom.displayValue.length).toDouble)
    val eq = d + amt
    eq
  }
  def sub(d : Length, value : String) : Length = {
    val uom = uomFromString(value);
    val amt = Length(uom, value.dropRight(uom.displayValue.length).toDouble)
    d - amt
  }
}

