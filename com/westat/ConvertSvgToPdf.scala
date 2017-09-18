package com.westat

import java.io.{ByteArrayOutputStream, ByteArrayInputStream, File}
import org.apache.batik.transcoder.{TranscoderOutput, TranscoderInput}
import org.apache.fop.svg.PDFTranscoder

/**
 * Created by Owner on 9/11/2017.
 * also needs the FOP 2.2 jar and all jars in the fop 2.2 lib directory
 * it does NOT require an xconf file!
 */

case class SVGRenderResult(svg : String, pdf : Array[Byte])

object ConvertSvgToPdf {

  def convertToFile(svg : String) = {
    val transcoder = new PDFTranscoder()
    val inStream = new ByteArrayInputStream(svg.getBytes("UTF-8"))
    val input = new TranscoderInput(inStream)

    val outStream = new java.io.FileOutputStream(new File("output.pdf"));
    val out = new java.io.BufferedOutputStream(outStream);

    val output = new TranscoderOutput(out)
    transcoder.transcode(input, output)
    outStream.close()
  }

  def convertToBA(svg : String) : SVGRenderResult = {
    val transcoder = new PDFTranscoder()
    val inStream = new ByteArrayInputStream(svg.getBytes("UTF-8"))
    val input = new TranscoderInput(inStream)

    val outStream = new ByteArrayOutputStream()
    val output = new TranscoderOutput(outStream)
    try {
      transcoder.transcode(input, output)
      outStream.close()
      SVGRenderResult(null, outStream.toByteArray)
    }
    catch {
      case e: Exception => {
        println(s"Error processing PDF: ${e.getClass.getName} ${e.getMessage}")
//        e.printStackTrace()
//        StringUtilities.writeFile("badsvg.svg", svg)
        SVGRenderResult(svg, null)
      }
    }
  }
}


