package soda.analysis

import scala.collection.mutable.ArrayDeque
import cz.vutbr.web.css.StyleSheet
import cz.vutbr.web.domassign.Analyzer.Holder
import cz.vutbr.web.css.NodeData
import cz.vutbr.web.domassign.AnalyzerUtil
import cz.vutbr.web.csskit.ElementMatcherSafeCS
import cz.vutbr.web.csskit.MatchConditionOnElements
import cz.vutbr.web.csskit.antlr4.CSSParserFactory
import cz.vutbr.web.csskit.antlr4.CSSParserFactory.SourceType
import java.net.URL
import cz.vutbr.web.css.RuleSet
import soda.utils.Util
import cz.vutbr.web.css.MediaSpec
import soda.dom._
import soda.poc._
import java.awt.Color

object Analysis {
  
  def getStyleSheets(dom: RenderableDocument, userCSS: String, url: URL) = {
    // TODO: br is supposed to be an inline element
    // val cssNorm = "head {display: none} body, div, p { display: block; margin:1em } body {color: #000000; background: #ffffff} br {display:block}"
    val domStyles = getDomStyles(dom)
    (List(CSSNorm.defaultStyleSheet, userCSS) ++ domStyles ).map(parseCSS(_, url))
  }

  def getDomStyles(dom: RenderableDocument): List[String] = {
    val styleElements = dom.getElementsByTagName("style")
    var i = 0
    val N = styleElements.getLength
    var list = ArrayDeque[String]()
    while (i < N) {
      list :+= styleElements.item(i).asInstanceOf[RenderableElement].getTextContent()
      i += 1
    }
    list.toList
  }

  def parseCSS(css:String, baseURL: URL) : StyleSheet = {
    CSSParserFactory.getInstance().parse(
      css,
      (url) => {url.openStream},
      "utf-8", SourceType.EMBEDDED, baseURL)
  }

  def getNodeData(element: RenderableElement, classifiedRules: Holder, parentNodeData: Option[NodeData]) = {
    val ruleSet = convertAttributeToStyle(element) ++ getInlineStyle(element)
    val rules = AnalyzerUtil.getApplicableRules(element, classifiedRules, ruleSet.toList.toArray)
    val xmlMatcher = new ElementMatcherSafeCS()
    val nodeData = AnalyzerUtil.getElementStyle(element, null, xmlMatcher, new MatchConditionOnElements(), rules)

    parentNodeData.foreach {pnd =>
      nodeData.inheritFrom(pnd)
      nodeData.concretize()
    }

    nodeData
  }

  def convertAttributeToStyle(element: RenderableElement) = {
    val styleStr = element.getTagName().toUpperCase() match {
      case "IMG" => getDimensionalStyle(element)
      case _ => ""
    }
    if (styleStr.length() > 0) {
      val parsed = parseSimpleStyle(styleStr, element, false)
      parsed.map(_.asInstanceOf[RuleSet])
    } else {
      None
    }
  }

  def getDimensionalStyle(el: RenderableElement) = {
    var attrs = ""

    val widthNode = el.getAttribute("width")
    if (widthNode != null) {
      attrs += "width: " + pixelise(widthNode) + ";"
    }

    val heightNode = el.getAttribute("height")
    if (heightNode != null) {
      attrs += "height: " + pixelise(heightNode) + ";"
    }

    attrs
  }

  private def pixelise(s: String) = {
    try {
      val ignored = Integer.parseInt(s);
      s + "px"
    } catch {
      case e : NumberFormatException => s
    }

  }

  def parseSimpleStyle(styleStr: String, element: RenderableElement, isInline: Boolean) = {
    val styleSheet = CSSParserFactory.getInstance().parse(styleStr, null, null, SourceType.INLINE, element, isInline, null)
    if (styleSheet.size > 0) {
      Some(styleSheet.get(0))
    } else {
      None
    }
  }

  private def getInlineStyle(element: RenderableElement) = {
    val style = element.getAttribute("style")
    if ((style != null) && (style.length() != 0)) {
      parseSimpleStyle(style, element, true).map(_.asInstanceOf[RuleSet])
    } else {
      None
    }
  }
}

sealed trait DecoratedNode {
  var children: Vector[DecoratedNode] = Vector.empty
  def childElements = children.collect { case en : ElementNode => en }

  def computeSelfL1Props(parent: DecoratedNode): Unit

  def computeL1Props(parent: DecoratedNode = null):Unit = {
    computeSelfL1Props(parent)
    children.foreach(_.computeL1Props(this))
  }

}

class DocumentNode(val doc: RenderableDocument) extends DecoratedNode {
  override def toString = "Doc\n" + children.map(_.toString).mkString("\n")
  def computedColor: Color = Color.BLACK
  def computeSelfL1Props(parent: DecoratedNode) = {}
  def computeSelfL2Props(parent: DecoratedNode) = {}
}

class ElementNode(val elem: RenderableElement, val nd: NodeData) extends DecoratedNode {
  override def toString = elem.getTagName() + "\n" + children.map(_.toString).mkString("\n")

  // Level 1 Properties
  val displayProp = new L1StringProp("display", "inline")
  val floatProp = new L1StringProp("float", "none")
  val positionProp = new L1StringProp("position", "static")

  def computeSelfL1Props(parent: DecoratedNode) = {
    displayProp.init(nd)
    floatProp.init(nd)
    positionProp.init(nd)

    val someNone = Some("none")

    // Section 9.7: Relationship between display, float and position
    if (displayProp.specified != someNone) {
      if (positionProp.specified == Some("absolute") || positionProp.specified == Some("fixed")) {
        floatProp.computed = someNone
        displayProp.computed = displayMapSection9_7(displayProp.specified)
      } else if (floatProp.specified != someNone || elem.isRootElem) {
        displayProp.computed = displayMapSection9_7(displayProp.specified)
      } else {
        displayProp.computed = displayProp.specified
      }
    } else {
      displayProp.computed = displayProp.specified
    }
  }

  def displayMapSection9_7(specified: Option[String]) = {
    specified match {
      case Some("inline-table") => Some("table")
      case Some("inline") => Some("block")
      case Some("table-row-group") => Some("block")
      case Some("table-column") => Some("block")
      case Some("table-column-group") => Some("block")
      case Some("table-header-group") => Some("block")
      case Some("table-footer-group") => Some("block")
      case Some("table-row") => Some("block")
      case Some("table-cell") => Some("block")
      case Some("table-caption") => Some("block")
      case Some("inline-block") => Some("block")
      case others => others
    }
  }
}

class PseudoNode(val belongsTo: ElementNode, val nd: NodeData) extends DecoratedNode {
  def computeSelfL1Props(parent: DecoratedNode) = ???
  def computeSelfL2Props(parent: DecoratedNode) = ???
}

class TextNode(val text: RenderableText, parent: Option[ElementNode]) extends DecoratedNode {
  override def toString = text.text
  def computeSelfL1Props(parent: DecoratedNode) = { }
  def computeSelfL2Props(parent: DecoratedNode) = { }
  def computedColor: Color = { null }
}

object Analyser {
  def process(url: java.net.URL) = {
    val dom = Util.parse(url)
    val styleSheets = Analysis.getStyleSheets(dom, "", url)
    val ddom = analyse(dom, styleSheets)
    // println(ddom)
    ddom
  }

  def analyse(dom: RenderableDocument, styleSheets: List[StyleSheet]): DocumentNode = {
    import scala.jdk.CollectionConverters._
    val classifiedRules = AnalyzerUtil.getClassifiedRules(styleSheets.asJava, new MediaSpec("screen"))
    analyse(dom, classifiedRules, None).asInstanceOf[DocumentNode]
  }

  def analyse(rn: RenderableNode, classifiedRules: Holder, parent: Option[ElementNode]): DecoratedNode = {
    rn match {
      case element: RenderableElement => {
        val nodeData = Analysis.getNodeData(element, classifiedRules, parent.map(_.nd))
        val dn = new ElementNode(element, nodeData)
        dn.children = element.children.map(c => analyse(c, classifiedRules, Some(dn))).toVector
        dn
      }
      case rtext: RenderableText =>
        new TextNode(rtext, parent)

      case rdoc: RenderableDocument => {
        val dn = new DocumentNode(rdoc)
        dn.children = rdoc.children.map(c => analyse(c, classifiedRules, None)).toVector
        dn
      }
    }
  }

}
