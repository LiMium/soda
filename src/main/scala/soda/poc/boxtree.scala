package soda.poc

import java.awt.Graphics2D
import java.awt.Color
import org.w3c.dom.Node
import soda.analysis.DecoratedNode
import soda.analysis.DocumentNode
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps

sealed trait BasicNode

sealed trait HasBox {
  val b: Box
}

sealed trait HasAbsChildren {
  def getAbsChildren: Vector[BoxTreeNode]
  def appendAbsChild(c: BoxTreeNode): Unit
}

sealed trait InlineSource {
  def isInflow: Boolean
  def initProps(vwProps: ViewPortProps):Unit
  def getInlineRenderables(vwProps: ViewPortProps) : Vector[Either[InlineRenderable, BoxTreeNode]]
  def computeRelativeOffsetsOfBoxes(vwProps: ViewPortProps): Unit
}

sealed trait BoxTreeNode extends HasBox with BasicNode {
  var isInflow = true
  var containingBlock: ContainingBlockRef = null

  var boxyDomChildren : Vector[BoxTreeNode] = null
  def boxyInflowChildren : Vector[BoxTreeNode] = if (boxyDomChildren != null) boxyDomChildren.filter(_.isInflow) else Vector.empty
  var inlinyDomChildren : Vector[InlineSource] = null

  def paint(g: Graphics2D): Unit
  def getInlineRenderables(vwProps: ViewPortProps) : Vector[Either[InlineRenderable, BoxTreeNode]]
  def dump(level: Int): String

  def computeL2Props(vwProps: ViewPortProps):Unit
  def computeRelativeOffsets(vwProps: ViewPortProps): Unit = {}
}

class TextRun(tn: TextNode, boxP: BoxWithProps ) extends InlineSource with BasicNode {
  val isInflow = true

  override def toString = tn.text.text
  def initProps(vwProps: ViewPortProps):Unit = {}

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

  def getInlineRenderables(vwProps: ViewPortProps) : Vector[Either[InlineRenderable, BoxTreeNode]] = {
    val words = split(tn.text.text)
    words.map(w => Left(new InlineWordRenderable(w, boxP.b.visibility, boxP.colorProp, boxP.fontProp))).toVector
  }

  def dump(level: Int): String = {
    "  " * level + "Text: '" + toString + "'"
  }
  def computeL2Props(vwProps: ViewPortProps) = {}
  def computeRelativeOffsetsOfBoxes(vwProps: ViewPortProps): Unit = {}
}

sealed trait InnerBoxType
case object InlineBoxType extends InnerBoxType
case object TableWrapperBoxType extends InnerBoxType
case object BlockContainerBoxType extends InnerBoxType
case object FlexContainerBoxType extends InnerBoxType
case object GridContainerBoxType extends InnerBoxType

object Util {
  def findCascadingOffsetY(boxP: BoxWithProps, cbRef: ContainingBlockRef, yOffset: Int):Int = {
    // println("Finding cascading y offset with ", boxP, yOffset, boxP.b.offsetY)
    val offsetY = yOffset + boxP.b.contentOffsetY
    if (boxP.b eq cbRef.cb.b) {
      offsetY
    } else {
      boxP.domParentBox.map(pb => findCascadingOffsetY(pb, cbRef, boxP.b.offsetY + offsetY)).getOrElse(offsetY)
    }
  }

  def findCascadingOffsetX(boxP: BoxWithProps, cbRef: ContainingBlockRef, xOffset: Int):Int = {
    // println("Finding cascading x offset with ", boxP, xOffset, boxP.b.offsetX)
    val offsetX = xOffset + boxP.b.contentOffsetX
    if (boxP.b eq cbRef.cb.b) {
      offsetX
    } else {
      boxP.domParentBox.map(pb => findCascadingOffsetX(pb, cbRef, boxP.b.offsetX + offsetX)).getOrElse(offsetX)
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

class AnonInlineBox(val textRun: TextRun, creator: BoxWithProps) extends InlineSource with BoxTreeNode {
  val b: Box = new Box
  def initProps(vwProps: ViewPortProps):Unit = {}

  def paint(g: java.awt.Graphics2D): Unit = {
    b.paint(g, null)
  }

  def getInlineRenderables(vwProps: ViewPortProps): Vector[Either[InlineRenderable, BoxTreeNode]] = {
    textRun.getInlineRenderables(vwProps)
  }

  def inlineLayout(vwProps: ViewPortProps): Unit = {
    getInlineRenderables(vwProps).foreach({
      case Left(ir) => inlinePseudoContext.addInlineRenderable(ir)
      case Right(_) =>
    })
    b.contentHeight = inlinePseudoContext.getHeight
    b.contentWidth = inlinePseudoContext.getWidth
  }

  val inlinePseudoContext = new InlinePseudoContext()
  def dump(level: Int): String = {
    ("  " * level) + "Anon inline\n" + textRun.dump(level + 1)
  }
  def computeL2Props(vwProps: ViewPortProps) = {}
  def computeRelativeOffsetsOfBoxes(vwProps: ViewPortProps): Unit = { }
}

// Root box will have parentBlock == None
class BoxWithProps(
  val b: Box,
  val elemNode: ElementNode,
  val domParentBox: Option[BoxWithProps]
  ) extends BoxTreeNode with HasAbsChildren with InlineSource {
  private var absChildren: Vector[BoxTreeNode] = Vector()
  def getAbsChildren: Vector[BoxTreeNode] = absChildren
  def appendAbsChild(c: BoxTreeNode): Unit = {
    absChildren :+= c
  }

  val tag = elemNode.elem.tag

  val id = elemNode.elem.getAttribute("id")
  val classAttrib = elemNode.elem.getAttribute("class")
  val debugId = "<" + tag + Option(id).map("#"+_).getOrElse("") + Option(classAttrib).map("."+_).getOrElse("") +">"

  val isRootElem = elemNode.elem.isRootElem
  val positionProp = elemNode.positionProp.get

  val isReplaced = (tag == "img") && (b.img != null)

  lazy val createsBFC = {
    val floatProp = elemNode.floatProp.get
    floatProp == "left" || floatProp == "right" || positionProp == "absolute" || positionProp == "fixed"
    // TODO: Add more conditions
  }

  // formatting context established by this box
  var formattingContext: Option[FormattingContext] = if (isRootElem || createsBFC) {
    if (isReplaced) {
    Some(new SimpleReplacedFormattingContext(this))
    } else {
    Some(new BlockFormattingContext(this))
    }
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
      case _ => ???
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
    if (boxyDomChildren != null) {
      boxyDomChildren.foreach(_.computeL2Props(vwProps))
    }
  }

  def initProps(vwProps: ViewPortProps):Unit = {
    computeL2Props(vwProps)
  }

  def paint(g: Graphics2D): Unit = {
    if (config.paintDebugLevel > 0) println("Painting, ", debugId)
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

    if (isReplaced && positionProp == "absolute") {
      // println("Directly painting box")
      // b.paint(gtc, bgColor)
    } else {
      if (inlineMode) {
        inlinePseudoContext.paint(gtc)
      } else {
        boxyInflowChildren.foreach(_.paint(gtc))
      }
      absChildren.foreach(_.paint(gt))
    }

    gtc.dispose()
    gt.dispose()
  }

  val displayComputed = elemNode.displayProp.computed.get
  val displayOuter = Util.displayOuterMap.getOrElse(displayComputed, displayComputed)
  val displayInner = Util.displayInnerMap.getOrElse(displayComputed, displayComputed)

  val blockLevel = displayOuter == "block"
  val innerBoxType = if (displayInner == "flow" || displayInner == "flow-root") {
    applicableFormattingContext.getFlowBoxType(displayOuter)
  } else {
    InlineBoxType
  }

  /*
  lazy val inlineMode = boxyInflowChildren.forall( {
    case boxP: BoxWithProps => boxP.displayOuter == "inline" || boxP.displayOuter == "run-in"
    case ab: AnonInlineBox => true
    case tr: TextRun => true
  })
  */
  lazy val inlineMode = inlinyDomChildren != null

  def getInlineRenderables(vwProps: ViewPortProps): Vector[Either[InlineRenderable, BoxTreeNode]] = {
    if (tag == "img") {
      if (b.img != null) {
        computePaddings(vwProps)
        if(!computeWidths()) {
          // use intrinsic
          b.contentWidth = b.img.getWidth
        }
        if(!computeHeights()) {
          // use intrinsic
          b.contentHeight = b.img.getHeight
        }
        Vector(Left(new InlineElemRenderable(b)))
      } else {
        Vector.empty
      }
    } else if (tag == "br") {
      // TODO: Remove this hack when pseudo elements are implemented
      Vector(Left(InlineBreak))
    } else {
      // inflowChildren.flatMap {_.getInlineRenderables(vwProps)}
      if (inlinyDomChildren != null) {
        inlinyDomChildren.flatMap (dc => {
          if (dc.isInflow) {
            dc.initProps(vwProps)
            dc.getInlineRenderables(vwProps)
          } else {
            dc match {
              case bp: BoxWithProps => Vector(Right(bp))
              case _ => ???
            }
          }
        })
      } else {
        boxyDomChildren.flatMap(_.getInlineRenderables(vwProps))
        // Vector.empty
      }
    }
  }

  def inlineLayout(heightUpdate: Boolean, vwProps: ViewPortProps): Unit = {
    // println("Doing inline layout in ", debugId, b.offsetY)
    inlinePseudoContext.maxWidth = b.contentWidth
    getInlineRenderables(vwProps).foreach({
      case Left(ir) => inlinePseudoContext.addInlineRenderable(ir)
      case Right(outOfFlow) => outOfFlow match {
        case boxP: BoxWithProps =>
        // println("Setting offset of inline out of flow", boxP)
          val cascadingOffsetY = Util.findCascadingOffsetY(this, boxP.containingBlock, inlinePseudoContext.currPos)
          val cascadingOffsetX = Util.findCascadingOffsetX(this, boxP.containingBlock, 0)
          boxP.b.offsetY = cascadingOffsetY
          boxP.b.offsetX = cascadingOffsetX
        case _ => ???
      }
    })
    if (heightUpdate) {
      b.contentHeight = inlinePseudoContext.getHeight
    }
    // b.contentWidth = inlinePseudoContext.getWidth
  }

  val inlinePseudoContext = new InlinePseudoContext()

  def dump(level: Int): String = {
    ("  " * level) + s"$debugId\n" + boxyDomChildren.map(_.dump(level + 1)).mkString("\n")
  }

  def containingWidth = containingBlock.width
  def containingHeight = containingBlock.height

  private def resolveLength(lengthSpec: LengthSpec, parentLength: Float) = {
    lengthSpec match {
      case AbsLength(pxs) => pxs.toInt
      case ParentRelLength(pct) => (pct * parentLength).toInt
      case frl: FontRelLength => frl.compute(fontProp).toInt
      case _ => 0
    }
  }

  def getComputedWidth = {
    size.width.specified match {
      case AutoLength => None
      case x => Some(resolveLength(x, containingWidth))
    }
  }

  def getComputedMinWidth = {
    resolveLength(size.minWidth.specified, containingWidth)
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
      case x => Some(resolveLength(x, containingWidth))
    }
  }

  def getComputedMaxWidth = {
    size.maxWidth.specified match {
      case NoneLength => None
      case x => Some(resolveLength(x, containingWidth))
    }
  }

  private def findRelativeOffset(factor:Int, parentLength: Float, prop: String, vwProps: ViewPortProps) = {
    Property.getSpec(elemNode.nd, prop).map(LengthProp.parseSpec(_)).map(resolveLength(_, parentLength)).map(_ * factor)
  }

  override def computeRelativeOffsets(vwProps: ViewPortProps) = {
    if (positionProp == "relative") {
      b.renderOffsetY = findRelativeOffset(1, containingHeight, "top", vwProps).orElse(findRelativeOffset(-1, containingHeight, "bottom", vwProps)).getOrElse(0)
      b.renderOffsetX = findRelativeOffset(1, containingWidth, "left", vwProps).orElse(findRelativeOffset(-1, containingWidth, "right", vwProps)).getOrElse(0)
    }
    boxyInflowChildren.foreach {_.computeRelativeOffsets(vwProps)}
    if (inlinyDomChildren != null) {
      inlinyDomChildren.foreach {_.computeRelativeOffsetsOfBoxes(vwProps)}
    }
  }

  private def getComputedPadding(ls: LengthSpec, vwProps: ViewPortProps) = {
    resolveLength(ls, containingWidth)
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
      case x => Some(resolveLength(x, containingWidth))
    }
    specWidthOpt.foreach {specWidth =>
      b.contentWidth = specWidth
    }
    specWidthOpt.isDefined
  }

  def computeHeights() = {
    lazy val containingHeight = containingBlock.height
    val specHeight = size.height.specified match {
      case AutoLength => None
      case x => Some(resolveLength(x, containingHeight))
    }
    specHeight match {
      case Some(pxs) => b.contentHeight = pxs
      case None =>
    }
    specHeight.isDefined
  }

  def computeRelativeOffsetsOfBoxes(vwProps: ViewPortProps): Unit = {
    computeRelativeOffsets(vwProps)
  }

  override def toString = debugId
}

sealed trait ContainingAreaType
case object WholeArea extends ContainingAreaType
case object PaddingArea extends ContainingAreaType
case object ContentArea extends ContainingAreaType

case class ContainingBlockRef(areaType: ContainingAreaType, cb: HasBox) {
  def width = cb.b.contentWidth
  def height = cb.b.contentHeight

  def addAsAbsoluteChild(btn: BoxTreeNode):Unit = {
    cb match {
      case b:HasAbsChildren => b.appendAbsChild(btn)
      case _ => println("TODO: Handle adding abs child to unknown node")
    }
  }
}

class InitialContainingBlock extends HasBox with HasAbsChildren {
  val b: Box = new Box()
  private var rootBox: BoxWithProps = null
  def setRootBox(box: BoxWithProps) = {
    rootBox = box
  }
  def appendAbsChild(c: BoxTreeNode): Unit = {rootBox.appendAbsChild(c)}
  def getAbsChildren: Vector[BoxTreeNode] = rootBox.getAbsChildren
}
