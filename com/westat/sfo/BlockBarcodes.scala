package com.westat.sfo

import com.westat.{Length, Location}

import java.io.ByteArrayOutputStream
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.krysalis.barcode4j.impl.int2of5.Interleaved2Of5Bean
import org.krysalis.barcode4j.output.svg.SVGCanvasProvider
//import org.krysalis.barcode4j.tools.UnitConv

/**
  * Created by lee on 7/26/17.
  *
  * Barcodes are implemented in this file.  They are Interleaved 2 of 5 and show the eye readable text.
  * Implemented using the barcode4j library, which makes SVG directly, but it doesn't offer a way to
  * directly specify the width that the barcode should fit into.
  *
  * By manipulating the ModuleWidth, which is the minimum barcode line width, you can affect the width,
  * but it is not possible to know or specify the total image width before you render it.
  */


case class BlockBarcode(graphicClass : String, width : Length, height : Length, spaceBefore : Length, spaceAfter : Length, data : String) extends PageBlock {
  def bottom : Length = Length.dimension("0fu")

  def displayString : String = {
    toString+"\n"
  }

  def isEmpty : Boolean = data.length == 0

  def toSVG(location: Location, paragraphs: Boolean): String = {
    graphicClass match {
       case "bar-code"  => drawBarCode(location)
       case _ => drawUnsupportedCode(location)
    }
  }

  def drawUnsupportedCode(location : Location) : String = {
    var line = location.top
    val sb = new StringBuilder(s"""<text x="${location.left.asInchesString}" y="${line.asInchesString}" """)
    sb.append("""style="font-size:10pt;stroke:none;fill:black;font-weight:lighter;font-family:Arial Bold;font-stretch:ultra-condensed;">""")
    sb.append(s"""<tspan x="${location.left.asInchesString}" y="${line.asInchesString}" >($graphicClass)</tspan>\n""")
    line = line + Length.NEW_LINE_SIZE
    sb.append(s"""<tspan x="${location.left.asInchesString}" y="${line.asInchesString}">is not supported</tspan>\n</text>""")
    sb.toString()
  }

  def drawBarCode(location : Location) : String = {
    val gen = new Interleaved2Of5Bean()
  //  gen.setHeight(UnitConv.in2mm(location.height.asInches.75))
    gen.setHeight((location.height / 2).asMillimetres)
    //  gen.setWideFactor(3)
    gen.setModuleWidth(.3)
    //   gen.setModuleWidth(UnitConv.pt2mm(2))
    val provider = new SVGCanvasProvider(false, 0)
    gen.generateBarcode(provider, data)

    val frag = provider.getDOMFragment
    val trans = TransformerFactory.newInstance().newTransformer()
    val domSrc = new DOMSource(frag)
    val bos = new ByteArrayOutputStream()
    val result = new StreamResult(bos)
    trans.transform(domSrc, result)
    bos.toString
    val res = bos.toString.drop(43)
    s"""<svg id="$graphicClass-$data" x="${location.left.asInchesString}" y="${location.top.asInchesString}" """ + res

    //      <svg height="17.8219mm" viewBox="0 0 24.99 17.8219" width="24.99mm" xmlns="http://www.w3.org/2000/svg">
    // x="2in" y="6in"
  }
}


