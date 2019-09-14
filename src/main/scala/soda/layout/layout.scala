package soda.layout

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Font
import java.awt.Graphics2D
import java.awt.image.BufferedImage

import cz.vutbr.web.css.NodeData
import cz.vutbr.web.css.RuleSet
import cz.vutbr.web.domassign.Analyzer.Holder
import soda.analysis.Analysis
import soda.dom.RenderableDocument
import soda.dom.RenderableElement
import soda.dom.RenderableNode
import soda.dom.RenderableText

final class Point {
  var x = 0f
  var y = 0f
}

final class Dimension {
  var width = 0f
  var height = 0f
  override def toString = s"$width x $height"
}

final class Thickness {
  var top = 0
  var bottom = 0
  var left = 0
  var right = 0

  def horizThickness = left + right
  def verticalThickness = top + bottom
}

final class LayoutData() {
  val position = new Point()
  val size = new Dimension()
  val bgColor: Color = null
}

case class FontKey(family: String, style: Int, size: Int)

final class RenderCtxt() {
  // val fontFamilies = GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
  def getFont(fKey: FontKey): Font = {
    new Font(fKey.family, fKey.style, fKey.size)
  }
  val defaultFontSize = 16
  val defaultFontFamily = "SansSerif"
  val defaultFont = getFont(FontKey(defaultFontFamily, Font.PLAIN, defaultFontSize))
}

sealed trait Renderable {
  val layoutData = new LayoutData()
  def paint(g: Graphics2D, ctxt: RenderCtxt):Unit
  def layout(avlWidth: Int, avlHeight: Int):Unit
  def getEstimatedWidth: Float
  val isBlock: Boolean
}

sealed trait DomRenderable extends Renderable {
  var children = List[Renderable]()
  val properties: Properties
  val parent: Option[DomRenderable]

  def getLevel: Int = parent.map(_.getLevel + 1).getOrElse(0)

  lazy val parentBlock = parent.flatMap(p => p match {
    case rb: RBox => Some(rb)
    case ri: RInline => Some(ri)
    case _ => None
  })
  def computeLength(size: Size, getContainerValue: () => Option[Float]) = {
    size match {
      case ConstantSize(pixels) => pixels
      case frs: FontRelativeSize => frs.computePixels(FontProps(16, 16, 16, 16), FontProps(16, 16, 16, 16))
      case crs: ContainerRelativeSize => crs.computePixels(getContainerValue().getOrElse(0f))
      case is: InheritSize => is.computePixels(getContainerValue().getOrElse(0f))
    }
  }

  lazy val declWidth: Option[Float] = {
    properties.getWidth.map(wp => computeLength(wp, () => parentBlock.flatMap(_.declWidth)))
  }

  lazy val declHeight: Option[Float] = {
    properties.getHeight.map(wp => computeLength(wp, () => parentBlock.flatMap(_.declHeight)))
  }

  lazy val declMinWidth = properties.getMinWidth.map(wp => computeLength(wp, () => parentBlock.flatMap(_.declWidth)))

  lazy val effectiveMinWidth: Option[Float] = declMinWidth.orElse(declWidth)

  lazy val declMinHeight = properties.getMinHeight.map(wp => computeLength(wp, () => parentBlock.flatMap(_.declHeight)))

  lazy val effectiveMinHeight: Option[Float] = declMinHeight.orElse(declHeight)

  lazy val declMaxWidth = properties.getMaxWidth.map(wp => computeLength(wp, () => parentBlock.flatMap(_.declWidth)))

  lazy val effectiveMaxWidth: Option[Float] = {
    val tentativeOpt = declMaxWidth.orElse(declWidth)
    tentativeOpt map { tentative =>
      declMinWidth.map(math.max(_ ,tentative)).getOrElse(tentative)
    }
  }

  lazy val declMaxHeight = properties.getMaxHeight.map(wp => computeLength(wp, () => parentBlock.flatMap(_.declHeight)))
  lazy val effectiveMaxHeight: Option[Float] = {
    val tentativeOpt = declMaxHeight.orElse(declHeight)
    tentativeOpt map { tentative =>
      declMinHeight.map(math.max(_ ,tentative)).getOrElse(tentative)
    }
  }

  def applyMinMaxConstraint(value: Float, min: Option[Float], max: Option[Float]) = {
    val minConstrained = min.map(m => math.max(m, value)).getOrElse(value)
    val maxConstrained = min.map(m => math.min(m, minConstrained)).getOrElse(minConstrained)
    maxConstrained
  }
}

trait BoxedRenderable extends Renderable {
}

trait ContainerRenderable extends DomRenderable {
  lazy val isBlock = properties.getDisplay == "block"
  var replaceables = List[Renderable]()
  val margins = new Thickness()
  val borderThicks = new Thickness()

  private def pixelise(s: String) = {
    try {
      val ignored = Integer.parseInt(s);
      s + "px"
    } catch {
      case e : NumberFormatException => s
    }

  }

  private def getDimensionalStyle(el: RenderableElement) = {
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

  private def convertAttributeToStyle(element: RenderableElement) = {
    val styleStr = element.getTagName().toUpperCase() match {
      case "IMG" => getDimensionalStyle(element)
      case _ => ""
    }
    if (styleStr.length() > 0) {
      val parsed = Analysis.parseSimpleStyle(styleStr, element, false)
      Some(parsed.asInstanceOf[RuleSet])
    } else {
      None
    }
  }

  def addNode(rn: RenderableNode, classifiedRules: Holder, parentNodeData: Option[NodeData]): Unit = {
    rn match {
      case element: RenderableElement =>
        val nodeData = Analysis.getNodeData(element, classifiedRules, parentNodeData)
        val parentProperties = parent.map(_.properties)
        val properties = new Properties(nodeData, parentProperties)
        val r = properties.getDisplay match {
          case "block" => new RBox(properties, Some(this))
          case "inline" => new RInline(properties, Some(this))
          case "inline-block" => new RInline(properties, Some(this))
          case "table" => new RBox(properties, Some(this))
          case "table-row" => new RBox(properties, Some(this))
          case null => new RInline(properties, Some(this))
          case _ => new RBox(properties, Some(this))
        }

        rn.getReplaceableRenderables(r).foreach {rr => r.replaceables :+= rr }

        rn.children.foreach { c =>
          r.addNode(c, classifiedRules, Some(nodeData))
        }
        children :+= r

      case rtext: RenderableText =>
        rtext.text.split("\\s").foreach { word =>
          children :+= new RWord(this, word)
        }

      case _: RenderableDocument => ???
    }
  }

  def parseColor(colorStr: String): Color = {
    if (colorStr(0) == '#') {
      val r = Integer.decode("0x" + colorStr.substring(1, 3))
      val g = Integer.decode("0x" + colorStr.substring(3, 5))
      val b = Integer.decode("0x" + colorStr.substring(5, 7))
      new Color(r, g, b)
    } else {
      Color.RED
    }
  }

  def paint(g: Graphics2D, ctxt: RenderCtxt) = {
    val g2 = g.create().asInstanceOf[Graphics2D]
    g2.translate(layoutData.position.x, layoutData.position.y)
    val bgColor = properties.getBackgroundColor
    if (bgColor != null) {
      g2.setPaint(parseColor(bgColor))
      g2.fillRect(0, 0, layoutData.size.width.toInt, layoutData.size.height.toInt)
    }
    drawBorders(g2)
    val fgColor = properties.getColor
    if (fgColor != null) {
      g2.setPaint(parseColor(fgColor))
    }
    val borderLeftWidth = borderThicks.left
    val borderTopHeight = borderThicks.top
    g2.translate(borderLeftWidth, borderTopHeight)
    replaceables.foreach { c => c.paint(g2, ctxt) }
    children.foreach { c => c.paint(g2, ctxt) }
    g2.dispose()
  }

  def drawBorders(g2: Graphics2D) = {
    val borderLeftWidth = borderThicks.left
    if (borderLeftWidth > 0) {
      val borderLeftColor = properties.getBorderColor("left")
      g2.setPaint(borderLeftColor)
      g2.setStroke(new BasicStroke(borderLeftWidth, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))
      g2.drawLine(borderLeftWidth/2, 0, borderLeftWidth/2, layoutData.size.height.toInt)
    }
    val borderRightWidth = borderThicks.right
    if (borderRightWidth > 0) {
      val borderRightColor = properties.getBorderColor("right")
      g2.setPaint(borderRightColor)
      g2.setStroke(new BasicStroke(borderRightWidth, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))
      val brx = layoutData.size.width - borderRightWidth/2
      g2.drawLine(brx.toInt, 0, brx.toInt, layoutData.size.height.toInt)
    }
    val borderTopHeight = borderThicks.top
    if (borderTopHeight > 0) {
      val borderTopColor = properties.getBorderColor("top")
      g2.setPaint(borderTopColor)
      g2.setStroke(new BasicStroke(borderTopHeight, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))
      g2.drawLine(0, borderTopHeight/2, layoutData.size.width.toInt, borderTopHeight/2)
    }
    val borderBottomHeight = borderThicks.bottom
    if (borderBottomHeight > 0) {
      val borderBottomColor = properties.getBorderColor("bottom")
      g2.setPaint(borderBottomColor)
      g2.setStroke(new BasicStroke(borderBottomHeight, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))
      val bby = layoutData.size.height - borderBottomHeight/2
      g2.drawLine(0, bby.toInt, layoutData.size.width.toInt, bby.toInt)
    }
  }
}

class RDocument extends ContainerRenderable {
  val properties = new Properties(null, null)
  val parent = None
  def layout(avlWidth: Int, avlHeight: Int) = {
    children.foreach { _.layout(avlWidth, avlHeight) }
  }

  def getEstimatedWidth: Float = ???

  /*
  override def paint(g: Graphics2D) = {
    g.setPaint(Color.LIGHT_GRAY)
    g.fillRect(0, 0, 400, 400)
    super.paint(g)
  }
  */
}

class Line {
  val items = collection.mutable.ArrayDeque[Renderable]()
  var y = 0f
  var currPosX = 0f
  def getMaxHeight: Float = {
    if (items.size > 0) {
      items.map(i => i.layoutData.size.height).max
    } else {
      0
    }
  }

  def addItem(r: Renderable) = {
    r.layoutData.position.x = currPosX
    r.layoutData.position.y = y
    items += r
    currPosX += r.layoutData.size.width
  }
}

// TODO: rename to RBlock
// TODO: add a container block parameter, perhaps in lieu of parent
class RBox(val properties: Properties, val parent: Option[DomRenderable]) extends BoxedRenderable with ContainerRenderable {

  val lines = collection.mutable.ArrayDeque[Line]()
  def computeLengthInt(size: Option[Size]) = size.map(computeLength(_, () => None).toInt).getOrElse(0)

  def layout(totalAvlWidth: Int, totalAvlHeight: Int) = {
    // println("Doing layout of RBox: " + properties)
    // val declaredWidth = properties.getWidth.map(computeLengthInt)
    // val avlWidth = declaredWidth.map(math.min(totalAvlWidth, _)).getOrElse(totalAvlWidth)
    val avlWidth = effectiveMaxWidth.getOrElse(totalAvlWidth.toFloat).toInt
    // val declaredHeight = properties.getHeight.map(computeLengthInt)
    // val avlHeight = declaredHeight.map(math.min(totalAvlHeight, _)).getOrElse(totalAvlHeight)
    val avlHeight = effectiveMaxHeight.getOrElse(totalAvlHeight.toFloat).toInt
    // val declaredHeight = properties.getHeight.map(computeLengthInt)

    borderThicks.left = computeLengthInt(properties.getBorderWidth("left"))
    borderThicks.right = computeLengthInt(properties.getBorderWidth("right"))
    borderThicks.top = computeLengthInt(properties.getBorderWidth("top"))
    borderThicks.bottom = computeLengthInt(properties.getBorderWidth("bottom"))
    var currLine = new Line()
    def startNewLine() = {
      val pastLine = currLine
      currLine = new Line()
      currLine.y = pastLine.y + pastLine.getMaxHeight
      lines += currLine
    }
    val subRenderables = children.flatMap {c =>
      c match {
        case ri:RInline => ri.getSubRenderables
        case _ => Seq(c)
      }
    }
    subRenderables.foreach { c =>
      c.layout(avlWidth, avlHeight)
      // if (c.properties.getDisplay == "block") {
      if (c.isBlock) {
        startNewLine()
      } else if (c.getEstimatedWidth + currLine.currPosX > avlWidth) {
        startNewLine()
      }
      val avlWidthForChild = avlWidth - currLine.currPosX
      c.layoutData.size.width = math.min(c.getEstimatedWidth, avlWidthForChild)
      currLine.addItem(c)
      if (c.isBlock) {
        startNewLine()
      }
    }
    val specifiedWidth = declWidth.getOrElse(avlWidth.toFloat)
    val maxLineWidth = if (lines.isEmpty) specifiedWidth else lines.map(_.currPosX).max
    layoutData.size.width = math.min(avlWidth, maxLineWidth).toInt
    layoutData.size.height = if (declHeight.isDefined) {
      avlHeight
    } else {
      math.min(avlHeight, borderThicks.verticalThickness + currLine.y + currLine.getMaxHeight)
    }
  }

  def getEstimatedWidth: Float = {
    borderThicks.horizThickness + effectiveMaxWidth.getOrElse(children.map(_.getEstimatedWidth).sum)
  }
}

class RInline(val properties: Properties, val parent: Option[DomRenderable]) extends BoxedRenderable with ContainerRenderable {
  def layout(totalAvlWidth: Int, totalAvlHeight: Int) = {
    val avlWidth = effectiveMaxWidth.getOrElse(totalAvlWidth.toFloat).toInt
    val avlHeight = effectiveMaxHeight.getOrElse(totalAvlHeight.toFloat).toInt
    children.foreach { c => c.layout(avlWidth, avlHeight) }
  }
  def getEstimatedWidth: Float = children.map(_.getEstimatedWidth).sum
  def getSubRenderables = replaceables ++ children
}

class RWord(parent: DomRenderable, text: String) extends Renderable {
  val isBlock = false
  // val properties = parent.get.properties
  // val properties = new Properties(null, parent.map(_.properties))
  val parentProps = parent.properties
  private def unquoteSingle(s: String): String = {
    if (s.length > 2) {
      if (s(0) == '\'' && s(s.length -1) == '\'') {
        s.substring(1, s.length-1)
      } else {
        s
      }
    } else {
      s
    }
  }

  private def compute(fontSize: FontSize) = {
    fontSize match {
      case ConstantFontSize(pixels) => pixels.toInt
      case _ => 24
    }
  }

  def paint(g: Graphics2D, ctxt: RenderCtxt) = {
    val fontSize = parentProps.getFontSize.map(compute).getOrElse(ctxt.defaultFontSize)
    val fontFamily = parentProps.getAsStringOpt("font-family").map(unquoteSingle(_)).getOrElse(ctxt.defaultFontFamily)
    g.setFont(ctxt.getFont(FontKey(fontFamily, Font.PLAIN, fontSize)))
    g.drawString(text, layoutData.position.x, layoutData.position.y + fontSize)
  }

  def layout(avlWidth: Int, avlHeight: Int) = {
    // TODO
    layoutData.size.height = 24
  }

  def getEstimatedWidth: Float = (text.length+2) * 8
  override def toString = "word: " + text
}

/* This class doesn't have its own properties. It's attached as a replaceable to
*  its parent renderable, and uses the parents properties directly */
class RImage(val container: DomRenderable, parentNode: RenderableNode, image: BufferedImage) extends Renderable {
  val isBlock = false
  // val properties = container.properties
  val expWidth = container.effectiveMaxWidth.getOrElse(image.getWidth.toFloat)
  val expHeight = container.declHeight.getOrElse(image.getHeight.toFloat)
  def paint(g: Graphics2D, ctxt: RenderCtxt) = {
    val w = layoutData.size.width.toInt
    val h = layoutData.size.height.toInt
    g.drawImage(image, layoutData.position.x.toInt, layoutData.position.y.toInt, w, h, null)
  }
  def layout(avlWidth: Int, avlHeight: Int) = {
    layoutData.size.width = expWidth
    layoutData.size.height = expHeight
    // println("image declWidth: " + declWidth)
  }
  def getEstimatedWidth: Float = expWidth
  override def toString = "img: "
}
