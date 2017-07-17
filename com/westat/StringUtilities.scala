package com.westat

import java.io._
import com.westat.gids.GidsFont
import sun.misc.{BASE64Decoder, BASE64Encoder}
import java.util.zip.Inflater
import scala.collection.mutable.ListBuffer

/**
 * Created by Lee on 9/14/2015.
 */

object StringUtilities {
  private var showDebug = false

  def deQuote(s : String) : String = {
    var result = s.trim
    if (result.indexOf("\"") == 0) {
      result = result.drop(1)
      if (result.lastIndexOf("\"") == result.length-1) result = result.dropRight(1)
    }
    result
  }

  def quoteStr(s : Any) : String = {
    "\"%s\"".format(s.toString)
  }

  def dbQuoteStr(s : Any) : String = {
    "'%s'".format(s.toString)
  }

  // returns a simple representation eg: ["one", "two", "three"]
  def listToQuotedList(aList : List[String]) : String = {
    var result = new ListBuffer[String]
    aList.foreach(k => result += quoteStr(k))
    result.mkString("[", ", ", "]")
  }

  // returns a simple representation eg: ["one", "two", "three"]
  def listToDBQuotedList(aList : List[String]) : String = {
    var result = new ListBuffer[String]
    aList.foreach(k => result += dbQuoteStr(k))
    result.mkString(",")
  }

  // its easy to get ByteArray from string aStr.getBytes()
  // but more flexible to pass ByteArray for large binaries from streams
  def base64Encode(value : Array[Byte]) : String = {
    new sun.misc.BASE64Encoder().encode(value)
  }

  def base64StreamEncode(inputStr : InputStream, outputStr : OutputStream) = {
    new BASE64Encoder().encodeBuffer(inputStr, outputStr)
  }

  def base64Decode(value : String) : String = {
    new String( new sun.misc.BASE64Decoder().decodeBuffer(value) )
  }

  def base64StreamDecode(inputStr : InputStream, outputStr : OutputStream) = {
    new BASE64Decoder().decodeBuffer(inputStr, outputStr)
  }

  private def shortenString(value : String, amt : Int) : String = {
    if (value.length > amt)
      return value.substring(0, amt)
    else
      value
  }

  def shortString(value : String) : String = {
    shortenString(value, 110)
  }

  def veryShortString(value : String) : String = {
    shortenString(value, 30)
  }
/*
  def jsonErrorString(msg : String) : String = {
    val res = Json.obj(
      "error" -> Json.obj(
        "msg" -> msg
      ))
    Json.stringify(res)
  }
*/
  def listOfMaxLengthStrings(src : String, maxLen : Int) : List[String] = {
    val res = new ListBuffer[String]
    val it = src. sliding(maxLen, maxLen)
    var rem : String = ""
    while (it.hasNext) {
      var n = rem + it.next()
      val x = n.lastIndexWhere(c => ",.- '?()!".contains(c))
      if (x != n.length) {
        rem = n.substring(x).replaceAll("^\\s+", "")
        n = n.substring(0, x+1)
      }
      else
        rem = ""
      res += n
    }
    if (rem.nonEmpty)
      res += rem
    res.toList
  }

  def listOfMaxLengthStrings(src : String, maxLen : Int, indentNum : Int) : List[String] = {
    val res = new ListBuffer[String]
    val it = src. sliding(maxLen, maxLen)
    var rem : String = "".padTo(indentNum, ' ')
    while (it.hasNext) {
      var n = rem + it.next()
      val x = n.lastIndexWhere(c => ",.- '?(),.!".contains(c))
      if (x != n.length) {
        rem = n.substring(x).replaceAll("^\\s+", "")
        n = n.substring(0, x+1)
      }
      else
        rem = ""
      res += n //.trim
    }
    if ( (rem.nonEmpty) && (rem.length > 1))
      res += rem
    res.toList
  }

  def chompStrings(src : String, firstLen : Int, maxLen : Int) : List[String] = {
    debug(s"chompStrings called first:$firstLen max:$maxLen for $src")
    val sl = new ListBuffer[String]
    var remainder = src
    if (firstLen >= src.length) {
      sl += src
      return sl.toList
    }

    var x = remainder.lastIndexOf(' ', firstLen)
    if (x > 0) {
      sl += remainder.substring(0, x)
      remainder = remainder.substring(x+1, remainder.length)
    }
    x = if (remainder.length < maxLen) {
      sl += remainder
      0
    }
    else
      remainder.lastIndexOf(' ', maxLen)
    while (x > 0) {
      sl += remainder.substring(0, x)
      remainder = remainder.substring(x+1, remainder.length)
      x = if (remainder.length < maxLen) {
        sl += remainder
        0
      }
      else
        remainder.lastIndexOf(' ', maxLen)
    }

    debugln("  "+sl.mkString("\n"))
    sl.toList
  }

  def fitIntoSpace(src : String, len : Length, font : GidsFont) : String = {
    if (font.stringWidth(src) < len)
      return src

    val charsAvail = Length.dimension( (len.asPoints / font.adjustedFontsize ).toString+"pt").asPoints.toInt
    var x = src.lastIndexOf(' ', charsAvail)
    var result = ""
    if (x > 0)
      result = src.substring(0, x)
    else
      return src
    if (result.trim == src.trim)
      return src
    var lastx = x
    var lastResult = result
    while (font.stringWidth(result) < len) {
      val nextWord = src.indexOf(' ', lastx+1)
      if (nextWord > 0)
        result = result + " " + src.substring(lastx+1, nextWord)
      // if latest is overflow, you must return last good string
      if ((font.stringWidth(result) >= len) || (result.trim == lastResult.trim)) {
        return lastResult
      }
      lastx = nextWord
      lastResult = result
    }
    result
  }

  def fitStringToLengths(src : String, firstLen : Length, maxLen : Length, font : GidsFont) : List[String] = {
    debug(s"fitStringToLengths called first:$firstLen max:$maxLen for $src")
    var line = fitIntoSpace(src, firstLen, font)
    val sl = new ListBuffer[String]
    sl += line
    if (line.length == src.length)
      return sl.toList
    var remainder = src.substring(line.length+1)
    while (remainder.length > 0) {
      line = fitIntoSpace(remainder, maxLen, font)
      sl += line
      if (line == remainder)
        remainder = ""
      else
        remainder = remainder.substring(line.length+1)
    }
    sl.toList
  }

  def debug(text : String) = {
    if (showDebug)
      print(text)
  }

  def debugln(text : String) = {
    if (showDebug)
      println(text)
  }

  def expandByteArray(arr : Array[Byte]) : String = {
    val outStream = new ByteArrayOutputStream()
    val inflater = new Inflater()
    inflater.setInput(arr)
    var tmp = new Array[Byte](4*1024)
    try{
      while(!inflater.finished()) {
        val size = inflater.inflate(tmp)
        outStream.write(tmp, 0, size)
      }
    } catch {
      case ex : Exception => println(ex.getMessage)

    } finally {
      try{
        if (outStream != null)
          outStream.close()
      } catch {
        case ex : Exception => println(ex.getMessage)
      }
    }
    outStream.toString("UTF-16LE").drop(2)
  }

  def decodeByteArray(arr : Array[Byte]) : Array[Byte] = {
    val inStream = new ByteArrayInputStream(arr)
    val outStream = new ByteArrayOutputStream()
    base64StreamDecode(inStream, outStream)
    inStream.close()
    outStream.close()
    outStream.toByteArray
  }

  def decodeAndExpandByteArray(arr : Array[Byte]) : String = {
    expandByteArray( decodeByteArray(arr) )
  }

  def writeFile(filename : String, contents : String) = {
    val pw = new PrintWriter(new File(filename), "UTF-8")
    pw.write(contents)
    pw.close
  }
}
