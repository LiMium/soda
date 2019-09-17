package soda.poc

import java.awt.Graphics2D
import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import org.w3c.dom.Node
import soda.analysis.DecoratedNode
import soda.analysis.DocumentNode
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps
import java.net.URL

class Sides[T](initial: => T) {
  var top: T = initial
  var right: T = initial
  var bottom: T = initial
  var left: T = initial

  override def toString = s"$top;$right;$bottom;$left"

  def forEach(f: (String, T) => Unit) = {
    f("top", top)
    f("right", right)
    f("bottom", bottom)
    f("left", left)
  }
}

class SidesInt extends Sides(0) {
  def horiz = left + right
  def vert = top + bottom
}

case class Rect(x: Int, y: Int, width: Int, height: Int)

class Border {
  var thickness: Int = 0
  var color: Color = null
  var style: String = "none"

  def colorDump = if (color != null) s"RGB:${color.getRed},${color.getGreen},${color.getBlue}" else "transp"
  override def toString = if (style == "none") "[0]" else s"$thickness $style $colorDump"
}

class Box {
  val border = new Sides[Border](new Border) {

    def vert = top.thickness + bottom.thickness
    def horiz = left.thickness + right.thickness
  }

  val marginThickness = new SidesInt
  val paddingThickness = new SidesInt

  val startPos = new Position
  var img: BufferedImage = null

  var offsetX = 0
  var offsetY = 0

  var renderOffsetX = 0
  var renderOffsetY = 0

  var contentWidth = 0
  var contentHeight = 0
  var visibility: Boolean = true
  var overflowX: String = "visible"
  var overflowY: String = "visible"

  def contentOffsetX = marginThickness.left + border.left.thickness + paddingThickness.left
  def contentOffsetY = marginThickness.top + border.top.thickness + paddingThickness.top

  def marginWidth = marginThickness.horiz
  def borderWidth = border.horiz
  def paddingWidth = paddingThickness.horiz
  def borderPaddingWidth = borderWidth + paddingWidth
  def borderBoxWidth = borderPaddingWidth + contentWidth
  def marginBoxWidth = marginWidth + borderBoxWidth
  def marginBoxSansContentWidth = marginWidth + borderWidth + paddingWidth

  def marginHeight = marginThickness.vert
  def borderHeight = border.vert
  def paddingHeight = paddingThickness.vert
  def borderBoxHeight = borderHeight + paddingHeight + contentHeight
  def marginBoxHeight = marginHeight + borderBoxHeight
  def marginBoxSansContentHeight = marginHeight + borderHeight + paddingHeight

  def paint(g: Graphics2D, bgColor: Color): Unit = {
    if (visibility) {
      // g.setColor(backgroundColor.actual.get)
      // val borderWidth = border.left.actual.get + border.right.actual.get
      // val paddingWidth = padding.left.actual.get + padding.right.actual.get
      //
      if (bgColor != null) {
        g.setColor(bgColor)
        g.fillRect(marginThickness.left, marginThickness.top, borderBoxWidth, borderBoxHeight)
      }

      // Paint left border
      val borderRect = Rect(marginThickness.left, marginThickness.top, borderBoxWidth, borderBoxHeight)
      paintBorder(g, borderRect)

      if (img != null) {
        val w = img.getWidth
        val h = img.getHeight
        // g.drawImage(img, 0, 0, w, h, null)
        g.drawImage(img, 0, 0, contentWidth, contentHeight, null)
      }
    }
  }

  private def paintBorder(g: Graphics2D, br: Rect) = {
    // println("Painting border: " + br + " with thickness: " + borderThickness + " color: " + borderColor)
    paintBorderHoriz(g, border.top.color, border.top.style, br.x, br.y, br.width, border.top.thickness)
    paintBorderHoriz(g, border.bottom.color, border.bottom.style, br.x, br.y+br.height - border.bottom.thickness, br.width, border.bottom.thickness)

    paintBorderVert(g, border.left.color, border.left.style, br.x, br.y, br.height, border.left.thickness)
    paintBorderVert(g, border.right.color, border.right.style, br.x+br.width - border.right.thickness, br.y, br.height, border.right.thickness)
  }

  private def paintBorderHoriz(g: Graphics2D, color: Color, style: String, x: Int, y: Int, width: Int, thickness: Int) = {
    if (width > 0 && thickness > 0) {
      style match {
        case "none" =>
        case "solid" => {
          g.setColor(color)
          for (i <- 0 until thickness) {
            g.drawLine(x, y+i, x+width-1, y+i)
          }
        }
        case _ =>
      }
    }
  }

  private def paintBorderVert(g: Graphics2D, color: Color, style: String, x: Int, y: Int, height: Int, thickness: Int) = {
    if (height > 0 && thickness > 0) {
      style match {
        case "none" =>
        case "solid" => {
          g.setColor(color)
          for (i <- 0 until thickness) {
            g.drawLine(x+i, y, x+i, y + height-1)
          }
        }
        case _ =>
      }
    }
  }

}

trait InlineRenderable {
  def paint(g: Graphics2D): Unit
  val estWidth: Int
  val estHeight: Int
  val box: Box
  val isBreak: Boolean
  val isSpace: Boolean = false
}

object InlineBreak extends InlineRenderable {
  def paint(g: Graphics2D): Unit = {}
  val estWidth: Int = 0
  val estHeight: Int = 0
  val box: Box = null
  val isBreak: Boolean = true
}

class InlineElemRenderable(val box: Box) extends InlineRenderable {
  // override def toString = "box width: " + boxO.contentWidth
  override def toString = s"box est: $estWidth x $estHeight"
  def paint(g: Graphics2D): Unit = {
    val gt = g.create(box.offsetX, box.offsetY, box.marginBoxWidth, box.marginBoxHeight).asInstanceOf[Graphics2D]
    box.paint(gt, null)
    gt.dispose()
  }
  val isBreak: Boolean = false

  val estWidth = box.marginBoxWidth
  val estHeight = box.marginBoxHeight
}

class InlineWordRenderable(word: String, visibility: Boolean, colorProp: ColorProp, fontProp: FontProp) extends InlineRenderable {
  override def toString = s"word '$word' est: $estWidth x $estHeight"
  val box = new Box()
  def paint(g: Graphics2D): Unit = {
    if (visibility) {
      g.setColor(colorProp.computed)
      g.setFont(fontProp.font)
      g.drawString(word, box.offsetX, box.offsetY + fontProp.size)
    }
  }

  val estWidth = fontProp.estWidth(word)
  val estHeight = fontProp.size
  val isBreak: Boolean = false
  override val isSpace: Boolean = word == " "
}

class Line(val yPos: Int) {
  var renderables = Vector[InlineRenderable]()
  private val space = 4
  var height = 0
  var width = 0

  def willFit(ir: InlineRenderable, maxWidth: Int) = {
    val requiredSpace = ir.estWidth
    (requiredSpace + width) <= maxWidth
  }

  private def shouldIgnore(ir: InlineRenderable) = {
    if (ir.isSpace) {
      renderables.lastOption.map(_.isSpace).getOrElse(true)
    } else {
      false
    }
  }

  def add(ir: InlineRenderable) = {
    if (!shouldIgnore(ir)) {
      ir.box.offsetX = width
      ir.box.offsetY = yPos

      val iwidth = ir.estWidth
      height = math.max(height, ir.estHeight)
      width += iwidth
      renderables = renderables.appended(ir)
    }
  }
}

class InlinePseudoContext {
  var maxWidth = 0
  var lines = Vector[Line]()
  var currLine:Line = null
  var currPos = 0

  private def startNewLine() = {
    if (currLine != null) {
      currPos += currLine.height
    }
    currLine = new Line(currPos)
    lines = lines.appended(currLine)
  }

  def addInlineRenderable(ir: InlineRenderable) = {
    if (ir.isBreak) {
      startNewLine()
    } else {
      if (currLine == null) {
        startNewLine()
      } else if (currLine.width != 0 && !currLine.willFit(ir, maxWidth)) {
        startNewLine()
      }
      currLine.add(ir)
    }
  }

  def paint(g: Graphics2D): Unit = {
    lines.foreach {l =>
      l.renderables.foreach{_.paint(g)}
    }
  }

  def getHeight = lines.map(_.height).sum
  def getWidth = lines.map(_.width).maxOption.getOrElse(0)
}


sealed trait BoxTreeNode {
  def paint(g: Graphics2D): Unit
  def generateBoxes(): Unit
  def getInlineRenderables : Vector[InlineRenderable]
  def dump(level: Int): String

  def computeL2Props(vwProps: ViewPortProps):Unit
  def computeRelativeOffsets(vwProps: ViewPortProps): Unit = {}
}

sealed trait HasBox {
  val b: Box
}

class TextRun(tn: TextNode, boxP: BoxWithProps ) extends BoxTreeNode {
  override def toString = tn.text.text

  def paint(g: Graphics2D): Unit = {
    /* TODO: Shouldn't come here
    g.setColor(boxP.colorProp.computed)
    g.setFont(boxP.fontProp.font)
    g.drawString(tn.text.text, 0, boxP.fontProp.size)
    */
  }

  def generateBoxes(): Unit = ???

  private def split(s: String) = {
    val replaced = s.replaceAll("(\\s)+", " ")
    val all = collection.mutable.ArrayBuffer[String]()
    val tokenizer = new java.util.StringTokenizer(replaced, " ", true)
    while (tokenizer.hasMoreTokens) {
      val token = tokenizer.nextToken
      all.append(token)
    }
    all.toList
  }

  def getInlineRenderables : Vector[InlineRenderable] = {
    val words = split(tn.text.text)
    words.map(new InlineWordRenderable(_, boxP.b.visibility, boxP.colorProp, boxP.fontProp)).toVector
  }

  def dump(level: Int): String = {
    "  " * level + "Text: '" + toString + "'"
  }
  def computeL2Props(vwProps: ViewPortProps) = {}
}

sealed trait InnerBoxType
case object InlineBoxType extends InnerBoxType
case object TableWrapperBoxType extends InnerBoxType
case object BlockContainerBoxType extends InnerBoxType
case object FlexContainerBoxType extends InnerBoxType
case object GridContainerBoxType extends InnerBoxType

object Util {
  def generateBoxElem(en: ElementNode, domParentBox: Option[BoxWithProps], containingBlock: HasBox): Option[BoxWithProps] = {
    val displayComputed = en.displayProp.computed
    val floatComputed = en.floatProp.computed
    val positionComputed = en.positionProp.computed
    if (displayComputed == Some("none")) {
      None
    } else {
      val box = new Box()
      if (en.elem.tag == "img") {
        val src = en.elem.getAttribute("src")
        val baseUrl = en.elem.getBaseURI()
        val imgUrl = new URL(new URL(baseUrl), src)
        box.img = ImageIO.read(imgUrl)
      }
      val boxWithProps = new BoxWithProps(box, en, domParentBox)
      boxWithProps.generateBoxes()
      boxWithProps.containingBlock = containingBlock
      Some(boxWithProps)
    }
  }

  val displayOuterMap = Map(
    "none" -> "none",
    "contents" -> "contents",
    "block" -> "block",
    "flow-root" -> "block",
    "inline" -> "inline",
    "inline-block" -> "inline",
    "run-in" -> "run-in",
    "list-item" -> "block",
    "inline list-item" -> "inline",
    "flex" -> "block",
    "inline-flex" -> "inline",
    "grid" -> "block",
    "inline-grid" -> "inline",
    "table" -> "block",
    "inline-table" -> "inline",
  )

  val displayInnerMap = Map(
    "none" -> "none",
    "contents" -> "contents",
    "block" -> "flow",
    "flow-root" -> "flow-root",
    "inline" -> "flow",
    "inline-block" -> "flow-root",
    "run-in" -> "flow",
    "list-item" -> "flow",
    "inline list-item" -> "flow",
    "flex" -> "flex",
    "inline-flex" -> "flex",
    "grid" -> "grid",
    "inline-grid" -> "grid",
    "table" -> "table",
    "inline-table" -> "table",
  )

}

class AnonInlineBox(val b: Box, val textRun: TextRun, creator: BoxWithProps) extends BoxTreeNode with HasBox {
  def generateBoxes(): Unit = ???
  def paint(g: java.awt.Graphics2D): Unit = {
    b.paint(g, null)
  }

  def getInlineRenderables: Vector[InlineRenderable] = {
    textRun.getInlineRenderables
  }

  def inlineLayout(): Unit = {
    getInlineRenderables.foreach(inlinePseudoContext.addInlineRenderable)
    b.contentHeight = inlinePseudoContext.getHeight
    b.contentWidth = inlinePseudoContext.getWidth
  }

  val inlinePseudoContext = new InlinePseudoContext()
  def dump(level: Int): String = {
    ("  " * level) + "Anon inline\n" + textRun.dump(level + 1)
  }
  def computeL2Props(vwProps: ViewPortProps) = {}
}

// Root box will have parentBlock == None and containingBlock == None
class BoxWithProps(
  val b: Box,
  val elemNode: ElementNode,
  val domParentBox: Option[BoxWithProps]
  ) extends BoxTreeNode with HasBox {

  val tag = elemNode.elem.tag

  val id = elemNode.elem.getAttribute("id")
  val classAttrib = elemNode.elem.getAttribute("class")
  val debugId = "<" + tag + Option(id).map("#"+_).getOrElse("") + Option(classAttrib).map("."+_).getOrElse("") +">"

  var containingBlock: HasBox = null
  var domChildren : Vector[BoxTreeNode] = null
  var inflowChildren : Vector[BoxTreeNode] = null

  val isRootElem = elemNode.elem.isRootElem
  val positionProp = elemNode.positionProp.get

  lazy val createsBFC = {
    val floatProp = elemNode.floatProp.get
    floatProp == "left" || floatProp == "right" || positionProp == "absolute" || positionProp == "fixed"
    // TODO: Add more conditions
  }

  // formatting context established by this box
  var formattingContext: Option[FormattingContext] = if (isRootElem || createsBFC) {
    Some(new BlockFormattingContext(this))
  } else {
    None
  }

  val applicableFormattingContext: FormattingContext = formattingContext.getOrElse(domParentBox.map(_.applicableFormattingContext).get)

  // Level2 Properties
  val backgroundColor = new ColorProp("background-color")
  val colorProp = new ColorProp("color")
  val fontProp = new FontProp()

  /*
  def getBorder(name: String) = name match {
    case "left" => borderLeft
    case "right" => borderRight
    case "top" => borderTop
    case "bottom" => borderBottom
  }
  */

  // Level 3 properties
  val size = new Size()

  // def computedColor: Color = { color.computed }

  def computeBorderProps(vwProps: ViewPortProps) = {
    val nd = elemNode.nd
    val currColor = colorProp.computed
    b.border.forEach{(name, side) =>
      val colorKey = s"border-$name-color"
      val styleKey = s"border-$name-style"
      val thickKey = s"border-$name-width"
      side.color = Property.getSpec(nd, colorKey).map(ColorProp.parseColor(_, currColor)).getOrElse(currColor)
      side.style = Property.getSpec(nd, styleKey).getOrElse("none")
      if (side.style != "none") {
        val specThick = Property.getSpec(nd, thickKey).getOrElse("medium")
        val specified = LengthProp.parseSpec(specThick, MediumLength)
        side.thickness = parseBorderWidth(specified, vwProps)
      }
    }
  }

  private def parseBorderWidth(spec: LengthSpec, vwProps: ViewPortProps) = {
    spec match {
      case AbsLength(l) => l.toInt
      case frl: FontRelLength => frl.compute(fontProp).toInt
      case MediumLength => vwProps.borderSizeMedium
      case ThinLength => vwProps.borderSizeThin
      case ThickLength => vwProps.borderSizeThick
    }
  }

  def computeSelfL2Props(vwProps: ViewPortProps) = {
    val nd = elemNode.nd
    val parentColor = domParentBox.map(_.colorProp.computed).getOrElse(Color.BLACK)
    // println(debugId + ": " + parentColor)
    colorProp.init(nd, parentColor)
    fontProp.init(nd, domParentBox.map(_.fontProp), vwProps)
    computeBorderProps(vwProps)
    backgroundColor.init(nd, null)
    b.visibility = parseVisibility()
    b.overflowX = Property.getSpec(nd, "overflow-x").getOrElse("visible")
    b.overflowY = Property.getSpec(nd, "overflow-y").getOrElse("visible")

  // }

  // def computeSelfL3Props(parent: DecoratedNode) = {
    size.init(nd)
  }

  private def parseVisibility() = {
    val specified = Property.getSpec(elemNode.nd, "visibility")
    specified map {
      case "hidden" => false
      case "collapse" => false
      case _ => true
    } getOrElse (domParentBox.map(_.b.visibility).getOrElse(true))
  }

  def computeL2Props(vwProps: ViewPortProps):Unit = {
    computeSelfL2Props(vwProps)
    domChildren.foreach(_.computeL2Props(vwProps))
  }

  def paint(g: Graphics2D): Unit = {
    val gt = g.create().asInstanceOf[Graphics2D]
    gt.translate(b.offsetX + b.renderOffsetX, b.offsetY + b.renderOffsetY)

    val bgColor = backgroundColor.specified
    b.paint(gt, bgColor)

    val gtc = gt.create().asInstanceOf[Graphics2D]
    val currClipBounds = gtc.getClipBounds
    gtc.translate(b.contentOffsetX, b.contentOffsetY)
    val clipBoundWidth = if (b.overflowX == "hidden" || b.overflowX == "scroll") b.contentWidth else currClipBounds.width
    val clipBoundHeight = if (b.overflowX == "hidden" || b.overflowX == "scroll") b.contentHeight else currClipBounds.height
    gtc.clipRect(currClipBounds.x, currClipBounds.y, clipBoundWidth, clipBoundHeight)

    if (inlineMode) {
      inlinePseudoContext.paint(gtc)
    } else {
      domChildren.foreach(_.paint(gtc))
    }

    gtc.dispose()
    gt.dispose()
  }

  def generateBoxes(): Unit = {
    domChildren = elemNode.children.flatMap(generateBoxNode)
  }

  val displayComputed = elemNode.displayProp.computed.get
  val displayOuter = Util.displayOuterMap.getOrElse(displayComputed, displayComputed)
  val displayInner = Util.displayInnerMap.getOrElse(displayComputed, displayComputed)

  val blockLevel = displayOuter == "block"
  val innerBoxType = if (displayInner == "flow" || displayInner == "flow-root") {
    applicableFormattingContext.getFlowBoxType(displayOuter)
  }

  lazy val inlineMode = domChildren.forall( {
    case boxP: BoxWithProps => boxP.displayOuter == "inline" || boxP.displayOuter == "run-in"
    case ab: AnonInlineBox => true
    case tr: TextRun => true
  })

  /*
  def adjustBoxes(): Unit = {
    domChildren = if (blockLevel) {
      ensureBlock(domChildren)
    } else {
      ensureInline(domChildren)
    }
  }
  */

  private def generateBoxNode(dn: DecoratedNode): Option[BoxTreeNode] = {
    dn match {
      case en: ElementNode => Util.generateBoxElem(en, Some(this), this)
      case tn: TextNode => {
        val textRun = new TextRun(tn, this)
        if (innerBoxType == InlineBoxType) {
          Some(textRun)
        } else {
          Some(new AnonInlineBox(new Box(), textRun, this))
        }
      }
      case _ => ???
    }
  }

  /*
  private def ensureBlock(childBoxes: Vector[BoxTreeNode]) : Vector[BoxTreeNode] = {
    var blocked = Vector[BoxTreeNode]()
    childBoxes.foreach {cb =>
      cb match {
        case boxWithProps : BoxWithProps => {
          if (boxWithProps.blockLevel) {
            blocked :+= boxWithProps
          } else {
            // TODO: combine adjacent inline boxes
            val anonBox = new Box()
            blocked :+= new BoxWithProps(anonBox, None, None, None)
          }
        }
        case _ =>
      }
    }
    blocked
  }

  private def ensureInline(childBoxes: Vector[BoxTreeNode]) : Vector[BoxTreeNode] = {
    var blocked = Vector[BoxTreeNode]()
    childBoxes.foreach { cb =>
      cb match {
        case boxWithProps : BoxWithProps => {
          if (boxWithProps.blockLevel) {
            blocked :+= cb
          } else {
            // TODO: combine adjacent inline boxes
            val anonBox = new Box()
            blocked :+= new BoxWithProps(anonBox, None, None, None)
          }
        }
        case _ =>
      }
    }
    blocked
  }
  */

  val isReplaced = (tag == "img") && (b.img != null)

  def getInlineRenderables: Vector[InlineRenderable] = {
    if (tag == "img") {
      if (b.img != null) {
        if(!computeWidths()) {
          // use intrinsic
          b.contentWidth = b.img.getWidth
        }
        if(!computeHeights()) {
          // use intrinsic
          b.contentHeight = b.img.getHeight
        }
        Vector(new InlineElemRenderable(b))
      } else {
        Vector.empty
      }
    } else if (tag == "br") {
      // TODO: Remove this hack when pseudo elements are implemented
      Vector(InlineBreak)
    } else {
      domChildren.flatMap {_.getInlineRenderables}
    }
  }

  def inlineLayout(heightUpdate: Boolean): Unit = {
    inlinePseudoContext.maxWidth = b.contentWidth
    getInlineRenderables.foreach(inlinePseudoContext.addInlineRenderable)
    if (heightUpdate) {
      b.contentHeight = inlinePseudoContext.getHeight
    }
    // b.contentWidth = inlinePseudoContext.getWidth
  }

  val inlinePseudoContext = new InlinePseudoContext()

  def dump(level: Int): String = {
    ("  " * level) + s"$debugId\n" + domChildren.map(_.dump(level + 1)).mkString("\n")
  }

  def containingWidth = containingBlock.b.contentWidth
  def containingHeight = containingBlock.b.contentHeight

  def getComputedWidth = {
    size.width.specified match {
      case AutoLength => None
      case AbsLength(pxs) => Some(pxs.toInt)
      case ParentRelLength(pct) => Some((pct * containingWidth).toInt)
      case frl: FontRelLength => Some(frl.compute(fontProp).toInt)
    }
  }

  def getComputedMinWidth = {
    size.minWidth.specified match {
      case AbsLength(pxs) => pxs.toInt
      case ParentRelLength(pct) => (pct * containingWidth).toInt
      case frl: FontRelLength => frl.compute(fontProp).toInt
    }
  }

  def getComputedMargin(sideStr: String) = {
    val side = sideStr match {
      case "left" => size.marginLeft
      case "right" => size.marginRight
      case "top" => size.marginTop
      case "bottom" => size.marginBottom
    }

    side.specified match {
      case AutoLength => None
      case AbsLength(pxs) => Some(pxs.toInt)
      case ParentRelLength(pct) => Some((pct * containingWidth).toInt)
      case frl: FontRelLength => Some(frl.compute(fontProp).toInt)
    }
  }

  def getComputedMaxWidth = {
    size.maxWidth.specified match {
      case NoneLength => None
      case AbsLength(pxs) => Some(pxs.toInt)
      case ParentRelLength(pct) => Some((pct * containingWidth).toInt)
      case frl: FontRelLength => Some(frl.compute(fontProp).toInt)
    }
  }

  private def findRelativeOffset(factor:Int, parentLength: Float, prop: String, vwProps: ViewPortProps) = {
    Property.getSpec(elemNode.nd, prop).map(LengthProp.parseSpec(_)). map {
      case AbsLength(pxs) => pxs.toInt
      case ParentRelLength(pct) => (pct * parentLength).toInt
      case frl: FontRelLength => frl.compute(fontProp).toInt
      case _ => 0
    }.map(_ * factor)
  }

  override def computeRelativeOffsets(vwProps: ViewPortProps) = {
    if (positionProp == "relative") {
      b.renderOffsetY = findRelativeOffset(1, containingHeight, "top", vwProps).orElse(findRelativeOffset(-1, containingHeight, "bottom", vwProps)).getOrElse(0)
      b.renderOffsetX = findRelativeOffset(1, containingWidth, "left", vwProps).orElse(findRelativeOffset(-1, containingWidth, "right", vwProps)).getOrElse(0)
    }
    domChildren.foreach {_.computeRelativeOffsets(vwProps)}
  }

  private def getComputedPadding(ls: LengthSpec, vwProps: ViewPortProps) = {
    ls match {
      case AbsLength(pxs) => pxs.toInt
      case ParentRelLength(pct) => (pct * containingWidth).toInt
      case frl: FontRelLength => frl.compute(fontProp).toInt
      case _ => 0
    }
  }

  def computePaddings(vwProps: ViewPortProps) = {
    b.paddingThickness.left = getComputedPadding(size.paddingLeft.specified, vwProps)
    b.paddingThickness.right = getComputedPadding(size.paddingRight.specified, vwProps)
    b.paddingThickness.top = getComputedPadding(size.paddingTop.specified, vwProps)
    b.paddingThickness.bottom = getComputedPadding(size.paddingBottom.specified, vwProps)
  }

  def computeWidths() = {
    val specWidthOpt = size.width.specified match {
      case AutoLength => if (isReplaced) None else Some(containingWidth - b.marginBoxSansContentWidth)
      case AbsLength(pxs) => Some(pxs.toInt)
      case ParentRelLength(pct) => Some((pct * containingWidth).toInt)
      case frl: FontRelLength => Some(frl.compute(fontProp).toInt)
    }
    specWidthOpt.foreach {specWidth =>
      b.contentWidth = specWidth
    }
    specWidthOpt.isDefined
  }

  def computeHeights() = {
    lazy val containingHeight = containingBlock.b.contentHeight
    val specHeight = size.height.specified match {
      case AutoLength => None
      case AbsLength(pxs) => Some(pxs.toInt)
      case ParentRelLength(pct) => Some((pct * containingHeight).toInt)
      case frl: FontRelLength => Some(frl.compute(fontProp).toInt)
    }
    specHeight match {
      case Some(pxs) => b.contentHeight = pxs
      case None =>
    }
    specHeight.isDefined
  }

  override def toString = debugId
}

class InitialContainingBlock() extends HasBox {
  val b: Box = new Box()
  def initBox(vwProps: ViewPortProps) = {
    b.contentWidth = vwProps.width
  }
}
