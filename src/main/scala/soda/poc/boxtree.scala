package soda.poc

import java.awt.Graphics2D
import java.awt.Color
import org.w3c.dom.Node
import soda.analysis.DecoratedNode
import soda.analysis.DocumentNode
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps
import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.ImageIO

sealed trait HasBox {
  val b: Box
}

trait CanPaint {
  def paint(g: Graphics2D): Unit
}

/*
trait Content extends CanPaint {
  val displayOuter: String
  // val position: String
  // val float: String

  def layout(vwProps: ViewPortProps): Unit
}

*/

sealed trait HasAbsChildren {
  def getAbsChildren: Vector[BoxTreeNode]
  def appendAbsChild(c: BoxTreeNode): Unit
}

sealed trait BoxTreeNode extends HasBox {
  // var miniContext: MiniContext[_] = null
  def getContents(parent: Content, vwProps: ViewPortProps): Vector[Content]

  def initProps(vwProps: ViewPortProps):Unit
  def computeRelativeOffsetsOfBoxes(vwProps: ViewPortProps): Unit

  var isInflow = true
  val isFlowRoot = false
  // var containingBlock: ContainingBlockRef = null

  var boxyDomChildren : Vector[BoxTreeNode] = Vector.empty
  def boxyInflowChildren : Vector[BoxTreeNode] = boxyDomChildren.filter(_.isInflow)
  // var inlinyDomChildren : Vector[BoxTreeNode] = null
  def inlinyDomChildren : Vector[BoxTreeNode] = boxyDomChildren

  def dump(level: Int): String

  def computeL2Props(vwProps: ViewPortProps):Unit
  def computeRelativeOffsets(vwProps: ViewPortProps): Unit = {}
}

sealed trait InnerBoxType
case object InlineBoxType extends InnerBoxType
case object TableWrapperBoxType extends InnerBoxType
case object BlockContainerBoxType extends InnerBoxType
case object FlexContainerBoxType extends InnerBoxType
case object GridContainerBoxType extends InnerBoxType

class AnonBox(val tn: TextNode, val creator: BoxWithProps) extends BoxTreeNode {
  def getContents(parent: Content, vwProps:ViewPortProps): Vector[Content] = {
    val words = getWords
    words match {
      case " " :: Nil => Vector.empty
      case _ => words.map(w => new InlineWordRenderable(parent, w, creator.b.visibility, creator.colorProp, creator.fontProp)).toVector
    }
  }

  val b: Box = new Box
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

  def getWords = split(tn.text.text)

  /*
  def paint(g: java.awt.Graphics2D): Unit = {
    b.paint(g, null)
    inlinePseudoContext.paint(g)
  }

  val inlinePseudoContext = new InlineMiniContext()
  */
  def dump(level: Int): String = {
    ("  " * level) + "Anon:\n" + tn
  }
  def computeL2Props(vwProps: ViewPortProps) = {}
  def computeRelativeOffsetsOfBoxes(vwProps: ViewPortProps): Unit = { }
}

// Root box will have parentBlock == None
class BoxWithProps(
  val b: Box,
  val elemNode: ElementNode,
  val domParentBox: Option[BoxWithProps]
  ) extends BoxTreeNode with HasAbsChildren {

    /*
  def getContents(): Vector[Content] = {
    println("Get contents in ", debugId)
    boxyDomChildren.flatMap( {
      case ab: AnonBox => ab.getContents()
      case bwp: BoxWithProps => if (bwp.displayOuter == "block") {
        Vector(new BlockContent (bwp) {
          def getFormattingContext() = Some(applicableFormattingContext)
          def layout(lc: LayoutConstraints): Unit = {
            applicableFormattingContext.innerLayout(this, lc)
          }
        })
      } else {
        bwp.getContents()
      }
    })
    /*
    if (displayOuter == "block") {
      Vector(new BlockContent (this) {
        def layout(vwProps: ViewPortProps): Unit = {
          applicableFormattingContext.layout(BoxWithProps.this, vwProps)
        }
      })
    } else {
      boxyDomChildren.flatMap(_.getContents())
    }
    */
  }
    */

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

  val img:BufferedImage = if (tag == "img") {
    val src = elemNode.elem.getAttribute("src")
    val baseUrl = elemNode.elem.getBaseURI()
    val imgUrl = new URL(new URL(baseUrl), src)
    ImageIO.read(imgUrl)
  } else {
    null
  }

  val isReplaced = (tag == "img") // && (b.img != null)

  val displayComputed = elemNode.displayProp.computed.get
  val displayOuter = BoxUtil.displayOuterMap.getOrElse(displayComputed, "block")
  val displayInner = BoxUtil.displayInnerMap.getOrElse(displayComputed, displayComputed)
  override val isFlowRoot = displayOuter == "flow-root"

  lazy val createsBFC = {
    val floatProp = elemNode.floatProp.get
    floatProp == "left" || floatProp == "right" || positionProp == "absolute" || positionProp == "fixed" || displayInner == "flow-root"
    // TODO: Add more conditions
  }

  private def getReplacedWidth:LengthSpec = {
    if (img != null) {
      size.width.specified match {
        case NoneLength => AbsLength(img.getWidth)
        case AutoLength => AbsLength(img.getWidth)
        case frl:FontRelLength => AbsLength(frl.compute(fontProp))
        case x => x
      }
    } else NoneLength
  }

  private def getReplacedHeight:LengthSpec = {
    if (img != null) {
      size.height.specified match {
        case NoneLength => AbsLength(img.getHeight)
        case AutoLength => AbsLength(img.getHeight)
        case frl:FontRelLength => AbsLength(frl.compute(fontProp))
        case x => x
      }
    } else NoneLength
  }

  // formatting context established by this box
  val formattingContext: Option[FormattingContext] = if (isRootElem || createsBFC) {
    if (isReplaced) {
      Some(new SimpleReplacedFormattingContext)
    } else {
      Some(new FlowFormattingContext(this))
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

  var overflowX = "visible"
  var overflowY = "visible"

  def computeSelfL2Props(vwProps: ViewPortProps) = {
    val nd = elemNode.nd
    val parentColor = domParentBox.map(_.colorProp.computed).getOrElse(Color.BLACK)
    // println(debugId + ": " + parentColor)
    colorProp.init(nd, parentColor)
    fontProp.init(nd, domParentBox.map(_.fontProp), vwProps)
    computeBorderProps(vwProps)
    backgroundColor.init(nd, null)
    b.visibility = parseVisibility()
    overflowX = Property.getSpec(nd, "overflow-x").getOrElse("visible")
    overflowY = Property.getSpec(nd, "overflow-y").getOrElse("visible")

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
    boxyDomChildren.foreach(_.computeL2Props(vwProps))
    if (inlinyDomChildren != null) {
      inlinyDomChildren.foreach {
        case bwp : BoxWithProps => bwp.computeL2Props(vwProps)
        case _ =>
      }
    }
  }

  def initProps(vwProps: ViewPortProps):Unit = {
    computeL2Props(vwProps)
  }

  def paint(g: Graphics2D): Unit = {
    ???
    /*

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

    // miniContext.paint(gtc)
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
    */
  }

  val blockLevel = displayOuter == "block"
  lazy val inlineMode = inlinyDomChildren != null

  def dump(level: Int): String = {
    ("  " * level) + s"$debugId\n" + boxyDomChildren.map(_.dump(level + 1)).mkString("\n")
  }

  // def containingWidth = containingBlock.width
  // def containingHeight = containingBlock.height

  private def resolveLengthForLayout(lengthSpec: LengthSpec):LengthSpec = {
    lengthSpec match {
      case frl: FontRelLength => AbsLength(frl.compute(fontProp))
      case x => x
    }
  }

  private def resolveLength(lengthSpec: LengthSpec, parentLength: Float) = {
    lengthSpec match {
      case AbsLength(pxs) => pxs.toInt
      case ParentRelLength(pct) => (pct * parentLength).toInt
      case frl: FontRelLength => frl.compute(fontProp).toInt
      case _ => 0
    }
  }

  /*
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
  */

  private def findRelativeOffset(factor:Int, parentLength: Float, prop: String, vwProps: ViewPortProps) = {
    Property.getSpec(elemNode.nd, prop).map(LengthProp.parseSpec(_)).map(resolveLength(_, parentLength)).map(_ * factor)
  }

  private def getComputedPadding(ls: LengthSpec, vwProps: ViewPortProps) = {
    resolveLengthForLayout(ls)
  }

  def computePaddings(paddingThickness: Sides[LengthSpec], vwProps: ViewPortProps) = {
    paddingThickness.left = getComputedPadding(size.paddingLeft.specified, vwProps)
    paddingThickness.right = getComputedPadding(size.paddingRight.specified, vwProps)
    paddingThickness.top = getComputedPadding(size.paddingTop.specified, vwProps)
    paddingThickness.bottom = getComputedPadding(size.paddingBottom.specified, vwProps)
  }

  /*
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
  */

  def computeRelativeOffsetsOfBoxes(vwProps: ViewPortProps): Unit = {
    computeRelativeOffsets(vwProps)
  }

  override def toString = debugId

  def getContents(aParent: Content, vwProps: ViewPortProps): Vector[Content] = {
    val border = b.border
    val paddingThickness = new Sides[LengthSpec](NoneLength)
    computePaddings(paddingThickness, vwProps)

    val marginSpecified = new Sides[LengthSpec](NoneLength);
    marginSpecified.top = resolveLengthForLayout(size.marginTop.specified)
    marginSpecified.bottom = resolveLengthForLayout(size.marginBottom.specified)
    marginSpecified.left = resolveLengthForLayout(size.marginLeft.specified)
    marginSpecified.right = resolveLengthForLayout(size.marginRight.specified)
    val widthSpecified = resolveLengthForLayout(size.width.specified)
    val heightSpecified = resolveLengthForLayout(size.height.specified)

    val offsets = new Sides[LengthSpec](NoneLength);
    def getOffset(name: String):LengthSpec = Property.getSpec(elemNode.nd, name).map(LengthProp.parseSpec(_)).getOrElse(AutoLength)
    offsets.top = resolveLengthForLayout(getOffset("top"))
    offsets.right = resolveLengthForLayout(getOffset("right"))
    offsets.bottom = resolveLengthForLayout(getOffset("bottom"))
    offsets.left = resolveLengthForLayout(getOffset("left"))

    val compMinWidth = resolveLengthForLayout(size.minWidth.specified)
    val compMaxWidth = resolveLengthForLayout(size.maxWidth.specified)

    val renderPropsComputed = new RenderProps(backgroundColor.computed, overflowX, overflowY)
    if (isReplaced) {
      if (displayOuter == "block") {
        if (tag == "img") {
          val painter = new CanPaint {
            var c: Content = _
            def paint(g: Graphics2D): Unit = {
              // g.drawImage(img, c.box.paintOffsetX, 0, c.box.contentWidth, c.box.contentHeight, null)
              g.drawImage(img, 0, 0, c.box.contentWidth, c.box.contentHeight, null)
            }
          }
          val c = new BlockContent(aParent, Some(painter), "blck replaced " + debugId, renderPropsComputed) {
              def getFormattingContext() = applicableFormattingContext
              def getSubContent() = Vector.empty
              val props = new LayoutProps(
                displayOuter, displayInner, positionProp,
                new Sides[LengthSpec](NoneLength), border, paddingThickness,
                getReplacedWidth, compMinWidth, compMaxWidth,
                getReplacedHeight, offsets)
              override def toString = "blk cntnt wrpr for " + debugId
            }
          painter.c = c
          Vector(c)
        } else {
          // TODO
          println("TODO: " + debugId)
          ???
        }
      } else {
        Vector(new InlineRenderable {
          val parent = aParent
          override def toString = "inline replaced " + debugId
          val isBreak: Boolean = false
          def paintSelf(g: Graphics2D): Unit = {
            // g.drawImage(img, box.paintOffsetX + box.contentOffsetX, box.paintOffsetY + box.contentOffsetY, box.contentWidth, box.contentHeight, null)
            g.drawImage(img, 0, 0, box.contentWidth, box.contentHeight, null)
          }
          val props = new LayoutProps(
            "inline", "flow", positionProp,
            new Sides[LengthSpec](NoneLength), ContentUtil.emptyBorder, paddingThickness,
            getReplacedWidth, compMinWidth, compMaxWidth,
            getReplacedHeight, offsets)
          val renderProps: RenderProps = renderPropsComputed
        })
      }
    } else {
      if (displayOuter == "block") {
        Vector(new BlockContent(aParent, None, debugId, renderPropsComputed) {
              def getFormattingContext() = applicableFormattingContext
              def getSubContent() = boxyDomChildren.flatMap(_.getContents(this, vwProps))
              val props = new LayoutProps(
                displayOuter, displayInner, positionProp,
                marginSpecified, border, paddingThickness,
                widthSpecified, compMinWidth, compMaxWidth,
                heightSpecified, offsets)
              override def toString = "blk cntnt wrpr for " + debugId
            }
        )
      } else {
        if (tag == "br") {
          Vector(new InlineBreak(aParent))
        } else {
          if (displayInner != "flow") {
            Vector(new InlineRenderable() {
              override def toString = "inline wrapper for " + debugId
              val parent: Content = aParent
              def paintSelf(g: Graphics2D): Unit = {
                /*
                val g2 = g.create().asInstanceOf[Graphics2D]
                g2.translate(box.offsetX, box.offsetY)
                g2.dispose()

                // miniContext.paint(g)
                */
              }
              val isBreak: Boolean = false
              override def getFormattingContext() = applicableFormattingContext
              override def getSubContent() = boxyDomChildren.flatMap(_.getContents(this, vwProps))

              val props = new LayoutProps(
                "inline", displayInner, positionProp,
                marginSpecified, border, paddingThickness,
                widthSpecified, compMinWidth, compMaxWidth,
                heightSpecified, offsets)

              val renderProps: RenderProps = renderPropsComputed
            })
          } else {
            boxyDomChildren.flatMap(_.getContents(aParent, vwProps))
          }
        }
      }
    }
  }
}

sealed trait ContainingAreaType
case object WholeArea extends ContainingAreaType
case object PaddingArea extends ContainingAreaType
case object ContentArea extends ContainingAreaType

/*
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
*/

class InitialContainingBlock extends HasBox with HasAbsChildren {
  val b: Box = new Box()
  private var rootBox: BoxWithProps = null
  def setRootBox(box: BoxWithProps) = {
    rootBox = box
  }
  def appendAbsChild(c: BoxTreeNode): Unit = {rootBox.appendAbsChild(c)}
  def getAbsChildren: Vector[BoxTreeNode] = rootBox.getAbsChildren
}

object BoxUtil {
/*
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
  */



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
