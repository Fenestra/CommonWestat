package com.westat.sfo

import java.io._
import java.util.zip._

import com.westat.StringUtilities

/**
  * Created by lee on 8/15/17.
  */

case class ExpansionObj(compressedSource : String) {
  private val expandedBA = decodeUnpackSource
  private val inStream = new ByteArrayInputStream(expandedBA)
  private val dataStream = new DataInputStream(inStream)
  private var lastIndex = 0
  dataStream.skipBytes(4)
  private var streamSize = inStream.available()

  def close = inStream.close()
  def position : Int = streamSize - inStream.available()

  def readByte : Byte = {
    dataStream.readByte()
  }

  // only 2 bytes long
  def readShortInt : Int = {
//    println("sintposition is " + position)
    var res = dataStream.readUnsignedByte()
    val num = dataStream.readUnsignedByte()
    res + (num * 256)
  }

  def readInt : Int = {
//    println("intposition is "+position)
    var res = dataStream.readUnsignedByte()/// .readInt()
    for (n <- 1 until 3) {
      val num = dataStream.readUnsignedByte()
      if (num != 0)
        res = res + (num * (256 * n))
    }
    inStream.skip(1)
    res
  }

  def readLong : Long = {
    dataStream.readLong()

/*    var result : Long = 0L //new Array[Byte](4)
    val outStream = new ByteArrayOutputStream
    val buf = new Array[Byte](4)
    inStream.read(buf, 0, 4)
    outStream.write(buf, 0, 4)
    val newStream = new ByteArrayInputStream(expandedBA)
//    result = objStream.readLong()
//    objStream.close
    outStream.close
//    new Long(result)
    result
    */
  }

  def readWideString : String = {
//    println("wsposition is " + position)
    val len = readInt * 2
    val buf = new Array[Byte](len)
    dataStream.read(buf)

    val sb = new StringBuilder
    for (x <- 0 until len) {
      if ((x % 2) == 0) {
        val c = buf(x).toChar
//        println(c)
        sb.append(c)

      }
    }
    sb.toString() //.getBytes("UTF-8").toString
  }

  def readString : String = {
//    println("sposition is "+position)
    val len = readInt
    val buf = new Array[Byte](len)
//println(s"reading length of $len pos now ${position}")
    dataStream.read(buf)
//   println(s"tostr ${buf.toString}")
    val sb = new StringBuilder
    for (x <- 0 until len) {
      sb.append(buf(x).toChar)
    }
    sb.toString
  }

  def safeReadString : String = {
    if (inStream.available() > 0) {
//     println("safeReadString is reading stream")
      readString
    }
    else
      ""
  }

  private def decodeUnpackSource : Array[Byte] = {
    val inStream = new ByteArrayInputStream(compressedSource.filterNot(c => c == ' ').getBytes)
    val outStream = new ByteArrayOutputStream
    val inflater = new InflaterOutputStream(outStream)
    StringUtilities.base64StreamDecode(inStream, inflater)
    inflater.flush()
    outStream.flush()
    inStream.close()
    inflater.close()
    outStream.close()
    outStream.toByteArray
  }

}


object ExpansionObj {

    /*  N is long   S is string   FS is TFontStyles  set of bold, italic, underline, strikeout
    angle
    text
    fontname
    color long
    size  long
    fontstyle  long
    height  int
    width   int
    if not at end
      fontcolorname

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

  def test(source : String) = {
    val exp = ExpansionObj(source)
    val angle = exp.readInt
    val text = exp.readWideString
    val fontname = exp.readString
    val color = exp.readInt // -26 3 times
    val size = exp.readInt
    println(s"size sb at 41 is $size")
    val fontstyle = exp.readInt //45 is actually 5 long because its a set!
    exp.readByte
    println(s"fontstyle sb at 45 is $fontstyle")
    val height = exp.readShortInt
    println(s"height sb at 50 is $height")
    val width = exp.readShortInt
    println(s"width sb at 52 is $width")
    val fontcolorname = exp.safeReadString //54
    println(s"test is angle:$angle text:$text font:$fontname color:$color size:$size fs:$fontstyle w:$width h:$height fc:$fontcolorname")
  }
}
/*
41  60 = <
42  0 =
43  0 =
44  0 =

45  0 =
46  0 =
47  0 =
48  0 =
49  0 =

50  0 =
51  0 =

52  0 =
53  0 =

54  7 = 
55  0 =
56  0 =
57  0 =
58  35 = #
59  69 = E
60  54 = 6
61  69 = E
62  54 = 6
63  69 = E
64  54 = 6

 */