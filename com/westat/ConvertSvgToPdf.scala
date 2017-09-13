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
    val inStream = new ByteArrayInputStream(svg.getBytes())
    val input = new TranscoderInput(inStream)

    val outStream = new java.io.FileOutputStream(new File("output.pdf"));
    val out = new java.io.BufferedOutputStream(outStream);

    val output = new TranscoderOutput(out)
    transcoder.transcode(input, output)
    outStream.close()
  }

  def convertToBA(svg : String) : SVGRenderResult = {
    val transcoder = new PDFTranscoder()
    val inStream = new ByteArrayInputStream(svg.getBytes())
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
        SVGRenderResult(svg, null)
      }
    }
  }
}



/*
import org.apache.fop.apps.FopFactory;
import org.apache.fop.apps.Fop;
import org.apache.fop.apps.MimeConstants;

/*..*/

// Step 1: Construct a FopFactory by specifying a reference to the configuration file
// (reuse if you plan to render multiple documents!)
FopFactory fopFactory = FopFactory.newInstance(new File("C:/Temp/fop.xconf"));

// Step 2: Set up output stream.
// Note: Using BufferedOutputStream for performance reasons (helpful with FileOutputStreams).
OutputStream out = new BufferedOutputStream(new FileOutputStream(new File("C:/Temp/myfile.pdf")));

try {
    // Step 3: Construct fop with desired output format
    Fop fop = fopFactory.newFop(MimeConstants.MIME_PDF, out);

    // Step 4: Setup JAXP using identity transformer
    TransformerFactory factory = TransformerFactory.newInstance();
    Transformer transformer = factory.newTransformer(); // identity transformer

    // Step 5: Setup input and output for XSLT transformation
    // Setup input stream
    Source src = new StreamSource(new File("C:/Temp/myfile.fo"));

    // Resulting SAX events (the generated FO) must be piped through to FOP
    Result res = new SAXResult(fop.getDefaultHandler());

    // Step 6: Start XSLT transformation and FOP processing
    transformer.transform(src, res);

} finally {
    //Clean-up
    out.close();
}


import java.io.ByteArrayOutputStream
import org.apache.batik.transcoder.{TranscoderOutput, TranscoderInput}
import org.apache.fop.svg.PDFTranscoder


/**
 * Created by Owner on 9/11/2017.
 */

object ConvertSvgToPdf {

  **
   * Converts an FO file to a PDF file using FOP
   * @param svg the SVG file
   * @param pdf the target PDF file
  // * @throws IOException In case of an I/O problem
  // * @throws TranscoderException In case of a transcoding problem
   *
  def convert(svg : String) : Array[Byte] = {
    val transcoder = new PDFTranscoder
//    val inStream = new ByteArrayInputStream(svg.getBytes())
    val outStream = new ByteArrayOutputStream()
//    base64StreamDecode(inStream, outStream)
    val input = new TranscoderInput(svg)
    val output = new TranscoderOutput((outStream))
    transcoder.transcode(input, output)
  //  inStream.close()
    outStream.close()
    outStream.toByteArray
  }
}

*
    //Create transcoder
    Transcoder transcoder = new PDFTranscoder();
    //Transcoder transcoder = new org.apache.fop.render.ps.PSTranscoder();

    //Setup input
    InputStream in = new java.io.FileInputStream(svg);
    try {
      TranscoderInput input = new TranscoderInput(in);

      //Setup output
      OutputStream out = new java.io.FileOutputStream(pdf);
      out = new java.io.BufferedOutputStream(out);
      try {
        TranscoderOutput output = new TranscoderOutput(out);

        //Do the transformation
        transcoder.transcode(input, output);
      } finally {
        out.close();
      }
    } finally {
      in.close();
    }
  }

  public static void convertFileNames(String svgName, String pdfName) {
    try {
      System.out.println("FOP ConvertSVG\n");
      System.out.println("Preparing...");

      //Setup input and output files
      File svgfile = new File(svgName);
      File pdffile = new File(pdfName);

      System.out.println("Input: SVG (" + svgfile + ")");
      System.out.println("Output: PDF (" + pdffile + ")");
      System.out.println();
      System.out.println("Transforming...");

      ConvertSVG app = new ConvertSVG();
      app.convertSVG2PDF(svgfile, pdffile);

      System.out.println("Success!");
    } catch (Exception e) {
      e.printStackTrace(System.err);
      System.exit(-1);
    }
  }

  **
   * Main method.
   * @param args command-line arguments
   *
  public static void main(String[] args) {
    try {
      System.out.println("FOP ConvertSVG\n");
      System.out.println("Preparing...");

      //Setup directories
      File baseDir = new File(".");
      File outDir = new File(baseDir, "out");
      outDir.mkdirs();

      //Setup input and output files
      File svgfile = new File(baseDir, "xml/svg/helloworld.svg");
      File pdffile = new File(outDir, "ResultSVG2PDF.pdf");

      System.out.println("Input: SVG (" + svgfile + ")");
      System.out.println("Output: PDF (" + pdffile + ")");
      System.out.println();
      System.out.println("Transforming...");

      ConvertSVG app = new ConvertSVG();
      app.convertSVG2PDF(svgfile, pdffile);

      System.out.println("Success!");
    } catch (Exception e) {
      e.printStackTrace(System.err);
      System.exit(-1);
    }
  }
}
*/

