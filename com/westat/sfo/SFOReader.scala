
package com.westat.sfo

import java.io.{File, PrintWriter}
import com.westat.gids.GidsFont
import com.westat.{Length, StringUtilities}
import scala.concurrent.Future
import scala.xml.{NodeSeq, XML, Node}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global


case class SFOReader(text : String) {
  private var xml : Node = null
  private var layoutMasterName : String = ""
  private var layoutMasterForm : String = ""
  var pageMasters : List[PageMaster] = null
  var items : List[QnrItem] = null
  var pageContents : List[PageContent] = null
  private var lastCol : Int = 0
  private var lastRow : Int = 0
  private var currentPM : PageMaster = null

  def readAll : Future[String] = {
    xml = XML.loadString(text).head
    getLayoutMasterSet
    // readrest, which is 
    //   if seqlayout getSequenceLayoutSet sequence-layout-set
    //   if widget getWidget  widget
    //   if pageSeq, showPageSequences
    getPageSequences
    Future(displayPageContentSVG)
  }

  def readFromFile : String = {
    xml = XML.loadFile(text).head
    getLayoutMasterSet
    getPageSequences
    writeSVGFromPageContent("test.svg")
    displayPageContentSVG
  }

  def writeSVG(id : String) : Future[String] = {
    xml = XML.loadString(text).head
    getLayoutMasterSet
    getPageSequences
    val filename = s"$id.svg"
    writeSVGFromPageContent("public/images/"+filename)
    Future(filename)
  }

  def readChildren(node : scala.xml.Node, indent : String = "") : SFOReader = {
    val ars = node.child
    ars.foreach(a => {
      val name = a.toString().substring(1, 50)
      readChildren(a, indent+" ")
    })
    this
  }

  def getLayoutMasterSet : SFOReader = {
    val lms = (xml \ "layout-master-set")
    layoutMasterName = (lms \ "@name").text
    layoutMasterForm = (lms \ "@form-number").text
    val pms = (lms \ "page-master")
// <sfo:page-master id="_558EFF5B-5ED6-4549-90D4-8640027C95A5_">
// <sfo:media color="white" form="custom" width="62412372fu" height="80768952fu" orientation="portrait"/>
    val pmList = new ListBuffer[PageMaster]
    pms.foreach(a => {
      val id = (a \ "@id").text
      val pcolor = (a \ "media" \ "@color").text
      val pwidth  = (a \ "media" \ "@width").text
      val pheight = (a \ "media" \ "@height").text
      val pm = PageMaster(id, pcolor, Length.dimension(pwidth), Length.dimension(pheight))
      pmList += pm
      getRegions(a, pm)
    })
   pageMasters = pmList.toList
   this
  }

  def getRegions(node : scala.xml.Node, pm : PageMaster) : SFOReader = {
    currentPM = pm
    val regs = (node \ "region")
    var regions = new ListBuffer[Region]  
    regs.foreach(r => {
      val left = (r \ "@left").text
      val top = (r \ "@top").text
      val width  = (r \ "@width").text
      val height = (r \ "@height").text
      val right = (r \ "@right").text
      val bottom = (r \ "@bottom").text
      val trans = (r \ "@transform").text
      val isback = (r \ "@is-background").text
      val reg = Region(Length.dimension(left), Length.dimension(top), 
                       Length.dimension(right), Length.dimension(bottom), 
                       Length.dimension(width), Length.dimension(height), trans, isback)
      regions += reg
      getRegionAreas(r, reg)
      reg.computeDimensions(pm)
    })
    pm.regions = regions.toList
    this
  }
  
  def createArea(node : scala.xml.Node, left : Length, top : Length) : Area = {
    val stacking = (node \ "@stacking").text
    val width  = (node \ "@width").text
    val height = (node \ "@height").text
    val name = (node \ "@content-name").text
    Area(stacking, left, top, Length.dimension(width), Length.dimension(height), name)
  }

  def getAreaAreas(node : scala.xml.Node, ar : Area) : SFOReader = {
    val ars = node.child.filter(c => c.toString().contains("sfo:area"))
    val sz = ars.size
    if (sz == 0) {
      return this
    }
    var areas = new ListBuffer[Area]
    var left = ar.left
    var top = ar.top
    ars.foreach(a => {
      val area = createArea(a, left, top)
      areas += area
      getAreaAreas(a, area)

      // after you assigned all the child area positions (in getAreaAreas),
      // set up the starting position for the next area parent you encounter
      if (area.stacking == "left")
        left = left + area.width
      if (area.stacking == "top")
        top = top + area.height
    })
    if (areas.nonEmpty)
      ar.areas = areas.toList
    this
  }

  // gets immediate children of region
  def getRegionAreas(node : scala.xml.Node, reg : Region) : SFOReader = {
    val ars = node.child
    var areas = new ListBuffer[Area]
    var left = reg.left
    var top = reg.top
    ars.filter(n => n.toString().contains("sfo:area")).foreach(a => {
      val area = createArea(a, left, top)
      areas += area
      getAreaAreas(a, area)
      if (area.stacking == "left")
         left = left + area.width
      if (area.stacking == "top")
         top = top + area.height
    })
    reg.areas = areas.toList
    this
  }

  def getPageSequences : SFOReader = {
    val seqs = (xml \ "page-sequence" )
    val psflows = new ListBuffer[QnrItem]
    seqs.foreach(a => {
      val name = (a \ "@title").text
      val pmid = (a \ "sequence-specification" \ "sequence-specifier-single" \ "@page-master-id").text
      pageMasters.find(pm => pm.id == pmid) match {
        case Some(pm) => currentPM = pm
        case None =>
      }
      getPageSeqFlows(a, psflows)
      // getStatics if you ever find out what sfo:static is...
    })
    items = psflows.toList
    this
  }

  def displayPageContentSVG : String = {
    pageContents.head.toSVG
  }

  def writeSVGFromPageContent(filename : String) = {
    val pw = new PrintWriter(new File(filename), "UTF-8")
    pw.write(displayPageContentSVG)
    pw.close
  }

  def makeQGraphic(name : String, graphicClass : String, width : String, height : String,
                   spaceBefore : String, spaceAfter : String, data : String) : Option[QnrItem] = {
    var col = Length.dimension("0in")
    var row = Length.dimension("0in")

    val matchingArea = currentPM.findLocation(name)
    if (matchingArea.ne(null)) {
      col = matchingArea.left
      row = matchingArea.top
    }
    else
      println("no matching area for "+name)

    val qi = QGraphic(name, col, row, graphicClass, Length.dimension(width), Length.dimension(height),
      Length.dimension(spaceBefore), Length.dimension(spaceAfter), data)
    Some(qi)
  }

  def makeQItem(name : String, text : String, cb : String, ab : String, font : GidsFont) : Option[QnrItem] = {
    if (text == "?")
      return None
    var col = Length.dimension("0in")
    var row = Length.dimension("0in")

    val matchingArea = currentPM.findLocation(name)
    if (matchingArea.ne(null)) {
      col = matchingArea.left
      row = matchingArea.top
    }
    else
      println("no matching area for "+name)

    var qi : QnrItem = null
    if (text.nonEmpty && text.ne("?"))
       qi = QItem(name, col, row, text.replaceFirst("^[?]", ""), font)
    if (cb.nonEmpty)
      qi = QCheckboxItem(name, col, row, cb, font)
    if (ab.nonEmpty)
      qi = QAnswerboxItem(name, col, row, ab, font)

    if (qi.ne(null))
      Some(qi)
    else
      None
  }

  def fontForNode(n : Node) : GidsFont = {
    val ffamily = (n \ "@font-family").text
    val fcolor = (n \ "@font-color").text
    val fsize = (n \ "@font-size").text
    val fweight = (n \ "@font-weight").text
    GidsFont.fontFromStrings(ffamily, fcolor, fsize, fweight)
  }

  def makePageContent(n : Node) : PageContent = {
    var pm : PageMaster = null
    val pmid = (n \ "sequence-specification" \ "sequence-specifier-single" \ "@page-master-id").text
    pageMasters.find(p => p.id == pmid) match {
       case Some(page) => pm = page
       case None => return null
     }
    PageContent((n \ "@title").text, pm)
  }

  def makeInlineText(n : Node, parentFont : GidsFont) : Option[InlineText] = {
    if (n.label != "inline")
      return None
    if (n.text == "?")
      return None
    val font = fontForNode(n)
    val text = if (font.notEqual(parentFont))
        InlineText(n.text, font)
      else
        InlineText(n.text, null)
    Some(text)
  }

  def makePageGraphic(n : Node) : Option[PageBlock] = {
    if (n.label != "block-graphic")
      return None
    val graphicClass = (n \ "@graphic-class").text
    val width  = Length.dimension((n \ "@width").text)
    val height = Length.dimension((n \ "@height").text)
    val spaceBefore = Length.dimension((n \ "@space-before").text)
    val spaceAfter = Length.dimension((n \ "@space-after").text)
    val data = (n \ "@data").text
    val pg = BlockGraphic(graphicClass, width, height, spaceBefore, spaceAfter, data)
    Some(pg)
  }

  def makePageBracket(n : Node) : Option[PageBlock] = {
    if (n.label != "block-bracket")
       return None
    val direction = (n \ "@direction").text
    val linewidth = Length.dimension((n \ "@line-width").text)
    val outlineColor = (n \ "@outline-color").text
    val size = Length.dimension((n \ "@size").text)
    val pb = BlockBracket(direction, linewidth, outlineColor, size)
    Some(pb)
  }

  //populate a top level block, that can have either blocks or inline as children or be empty
  //   maybe not - it looks like blocks only have inline as children
  def makePageBlock(n : Node) : Option[PageBlock] = {
    n.label match {
      case "block-graphic" => return makePageGraphic(n)
      case "block-bracket" => return makePageBracket(n)
      case _ =>
    }
    if (n.label != "block")
       return None
    if (n.text == "?")
       return None
    val font = fontForNode(n)
    val pb = BlockText(font, TextAlignments.valueForTextAlignString((n \ "@text-align").text))
    n.child.foreach(bc =>
      makeInlineText(bc, font) match {
        case Some(text) => pb.addText(text)
        case None =>
      })
    Some(pb)
  }

  def makePageFlow(n : Node, pm : PageMaster) : Option[PageFlow] = {
    if (n.label != "flow")
       return None
    if (n.child.isEmpty)
       return None

    val color = (n \ "@background-color").text
    val name = (n \ "@content-name").text
    var matchingArea = pm.findLocation(name)
    if (matchingArea == null)
       matchingArea = pm.location
    val pf = PageFlow(name, color, matchingArea)
    n.child.foreach(b => {
      makePageBlock(b) match {
        case Some(block) => pf.addBlock(block)
        case None =>
      }
    })
    Some(pf)
  }

  def makeContent(f : NodeSeq) = {
    val pagelist = new ListBuffer[PageContent]
    f.foreach(n => {
      val page = makePageContent(n)
      pagelist += page
      //      <sfo:page-sequence title="Form ASCO-L3_P01_08 - Page 0"><sfo:sequence-specification><sfo:sequence-specifier-single page-master-id="1E24480C-8F11-429C-8E4C-A6CF62A04D6E"/></sfo:sequence-specification>
      // grab the pagemaster id  look up pageseq title in areas will give you the location info  but grab the color from pagemaster
      for (elem <- n.child) {
        elem.foreach(cn => {
          makePageFlow(cn, page.pageMaster) match {
            case Some(flow) => page.addFlow(flow)
            case None =>
          }
        })}

    })
    pageContents = pagelist.toList
  }

  def getPageSeqFlows(f : NodeSeq, flowlist : ListBuffer[QnrItem]) : SFOReader = {
    makeContent(f)
    flowlist.clear()
    val flows = (f \ "flow")

    flows.foreach(f => {
      val name = (f \ "@content-name").text
      val graphicClass = (f \ "block-graphic" \ "@graphic-class").text
      if (graphicClass.nonEmpty) {
        val width  = (f \ "block-graphic" \ "@width").text
        val height = (f \ "block-graphic" \ "@height").text
        val spaceBefore = (f \ "block-graphic" \ "@space-before").text
        val spaceAfter = (f \ "block-graphic" \ "@space-after").text
        val data = (f \ "block-graphic" \ "@data").text.substring(1, 50)
        makeQGraphic(name, graphicClass, width, height, spaceBefore, spaceAfter, data) match {
          case Some(qi) => flowlist += qi
          case None =>
        }
      }
      else {
        val text = (f \ "block" \ "inline").text
        val cbde = (f \ "block-check-box" \ "@capture-info").text
        val abde = (f \ "block-answer-box" \ "@capture-info").text
        val aFont = GidsFont.nullFont
        makeQItem(name, text, cbde, abde, aFont) match {
          case Some(qi) => flowlist += qi
          case None =>
        }
      }
    currentPM.setContentItems(flowlist.toList)
    })
    this
  }
}

object SFOReader {
  def test = {
    val filename = "Converted-documentLayout.txt"
    SFOReader(filename).readAll
   }

  def calcTests = {
    var reg = Region(Length.dimension("1in"), Length.dimension("1in"), Length.dimension("6in"), Length.dimension("8in"),
       Length.dimension("5in"), Length.dimension("7in"), "", "")
    // get region areas, add list to reg
    var regAreas = new ListBuffer[Area]
    var subAreas = new ListBuffer[Area]
    var parArea = Area("top", reg.left, reg.top, null, Length.dimension("0.5in"), "half inch title")
    regAreas += parArea

    parArea = Area("top", reg.left, reg.top, null, Length.dimension("1.5in"), "first 1.5in top area group")
      var subArea = Area("left", parArea.left, parArea.top, Length.dimension("2in"), null, "first 2in left subgroup")
      subAreas += subArea

      subArea = Area("top", parArea.left, parArea.top, Length.dimension("0.5in"), null, "0.5in top spacer betwn subgroups")
      subAreas += subArea

      subArea = Area("left", parArea.left, parArea.top, Length.dimension("2in"), null, "second 2in left subgroup")
      subAreas += subArea

      parArea.areas = subAreas.toList

  regAreas += parArea
  reg.areas = regAreas.toList

  println("sb")
  println("region rect inches  1.0, 1.0    6.0, 8.0")
    println("area half inch title rect inches  1.0, 1.0    6.0, 1.5")
    println("area first 1.5in area group rect inches  1.0, 1.5    6.0, 3.0")
    println("area first 0.6in left subgroup rect inches  1.0, 1,5   1.6, 3.0")
    println("area second 1in left spacer subgroup rect inches  1.6, 3.0  2.6, 3.0")
    println("area second 0.6in left subgroup rect inches  2.6. 3.0  3.2, 4.5")
  }

}
