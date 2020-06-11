package soda.layout

import java.awt.Graphics2D
import java.awt.Color
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps
import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.ImageIO
import scala.collection.mutable

trait CanPaint {
  def paint(g: Graphics2D, box: Box): Unit
}

sealed trait BoxTreeNode {
  def getContents(parent: Content, vwProps: ViewPortProps): Vector[Content]

  def dump(level: Int): String

  def computeL2Props(vwProps: ViewPortProps):Unit
}

sealed trait BoxTreeNodeProps extends BoxTreeNode {
  val displayInner: String
  def appendChild(btn: BoxTreeNode): Unit
  def adjustChildren(): Unit
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
    words.map(w => new InlineWordRenderable(parent, w, creator.visibility, creator.colorProp, creator.fontProp, vwProps)).toVector
  }

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

  def isJustSpace = {
    val ws = getWords
    ws.forall(_.isBlank())
  }

  def dump(level: Int): String = {
    ("  " * level) + toString
  }

  override def toString = "Anon: " + getWords

  def computeL2Props(vwProps: ViewPortProps): Unit = {}
}

class AnonGeneratedBox(aProps: LayoutProps, fcOpt: Option[FormattingContext]) extends BoxTreeNodeProps {
  override def getContents(parent: Content, vwProps: ViewPortProps): Vector[Content] = {
    // TODO: Check displayOuter and create block or inline content appropriately
    val bgProps = new BackgroundProps()
    val rp = new RenderProps(bgProps, "visible", "visible", true)
    val fc = fcOpt.getOrElse(parent.getFormattingContext())
    val content = if (displayInner == "table") {
      new TableContent("Anon Gen Box for " + displayInner, aProps, rp, parent, domChildren, vwProps)
    } else {
      new BlockContent(parent, None, "anon gen box", rp) {
        def getFormattingContext() = fc
        def getSubContent() = domChildren.flatMap(_.getContents(this, vwProps))
        override def toString = "anon gen box"
        val props = aProps
      }
    }
    Vector(content)
  }

  val displayInner: String = aProps.displayInner

  def dump(level: Int): String =
    ("  " * level) + s"Anon gen box: ${aProps.displayInner}\n" + domChildren.map(_.dump(level + 1)).mkString("\n")

  override def computeL2Props(vwProps: ViewPortProps): Unit = {domChildren.foreach{_.computeL2Props(vwProps)}}

  private var domChildren = Vector[BoxTreeNode]()
  def appendChild(btn: BoxTreeNode): Unit = {
    domChildren :+= btn
  }
  def adjustChildren(): Unit = domChildren.foreach {case b: BoxTreeNodeProps => b.adjustChildren; case _ =>}

  override def toString(): String = s"Anon gen box: ${aProps.displayInner}"
}

// Root box will have parentBlock == None
class BoxWithProps(
  val elemNode: ElementNode,
  val domParentBox: Option[BoxWithProps]
  ) extends BoxTreeNodeProps {

  private var origDomChildren : Vector[BoxTreeNode] = Vector.empty
  def setBoxyDomChildren(children: Vector[BoxTreeNode]) = {
    origDomChildren = children
  }

  private var boxyDomChildren : Vector[BoxTreeNode] = Vector.empty
  def appendChild(btn: BoxTreeNode): Unit = {
    boxyDomChildren :+= btn
  }

  def adjustChildren(): Unit = {
    val parentStack = mutable.Stack[BoxTreeNodeProps]()
    parentStack.push(this)

    def popOne() = {
      if (parentStack.size > 1) {
        val popped = parentStack.pop
        parentStack.top.appendChild(popped)
      }
    }

    def needParent(level: Int, needDIs: List[String]):Unit = {
      // if (level == 0) {
      {
        val lastIndex = parentStack.lastIndexWhere(p => needDIs.contains(p.displayInner))
        if (lastIndex > 0) {
          // println(s"Found $needDIs at $lastIndex / $parentStack")
          for (i <- 0 until lastIndex) {
            popOne()
            // println(s"  after popping: $parentStack")
          }
        }
      }
      val create = !needDIs.contains(parentStack.top.displayInner)
      if (create) {
        // println(s"  needed $needDIs @level $level, stack: ${parentStack.map(_.displayInner)} ${parentStack.size}")
        if (needDIs.contains("table-cell")) {
          needParent(level+1, List("table-row"))
        } else if (needDIs.contains("table-row")) {
          needParent(level+1, List("table-row-group", "table-header-group", "table-footer-group"))
        } else if (needDIs.contains("table-row-group") || needDIs.contains("table-header-group") || needDIs.contains("table-footer-group") || needDIs.contains("table-column-group")) {
          needParent(level+1, List("table"))
        }
        val needDI = needDIs.head
        val props = new LayoutProps("block", needDI, "static",
          new Sides[LengthSpec](NoneLength), ContentUtil.emptyBorder, ContentUtil.emptyOffsets,
          AutoLength, ContentUtil.zeroLength, NoneLength,
          NoneLength, fontProp, ContentUtil.emptyOffsets)
        val fcOpt = None
        val anonGenBox = new AnonGeneratedBox(props, fcOpt)
        // println("    pushing anon: " + needDI + " on top of " + parentStack.map(_.displayInner))
        parentStack.push(anonGenBox)
      }
    }

    def popSibling(siblingDI: String) = {
      if (parentStack.top.displayInner == siblingDI) {
        // println("Popping sibling: " + siblingDI)
        popOne()
      }
    }

    def addWithChecks(child: BoxTreeNodeProps): Unit = {
      // println(s"In $debugId $displayInner, adding child: $child ${child.displayInner}")
      if (child.displayInner == "table-caption") {
        needParent(0, List("table"))
      } else if (child.displayInner == "table-column") {
        needParent(0, List("table-column-group"))
      } else if (child.displayInner == "table-column-group") {
        needParent(0, List("table"))
      } else if (child.displayInner == "table-cell") {
        popSibling("table-cell")
        needParent(0, List("table-row"))
      } else if (child.displayInner == "table-row") {
        popSibling("table-column-group")
        popSibling("table-row")
        needParent(0, List("table-row-group", "table-header-group", "table-footer-group"))
      } else if (child.displayInner == "table-row-group") {
        popSibling("table-column-group")
        popSibling("table-row")
        popSibling("table-row-group")
        popSibling("table-header-group")
        popSibling("table-footer-group")
        needParent(0, List("table"))
      } else if (child.displayInner == "table-header-group") {
        popSibling("table-column-group")
        popSibling("table-row")
        popSibling("table-row-group")
        popSibling("table-header-group")
        popSibling("table-footer-group")
        needParent(0, List("table"))
      } else if (child.displayInner == "table-footer-group") {
        popSibling("table-column-group")
        popSibling("table-row")
        popSibling("table-row-group")
        popSibling("table-header-group")
        popSibling("table-footer-group")
        needParent(0, List("table"))
      } else {
        if (BoxUtil.tableCellContainers.contains(parentStack.last.displayInner)) {
          // popSibling("table-cell")
          needParent(0, List("table-cell"))
        } else {
          popSibling("table-column-group")
          popSibling("table-cell")
          popSibling("table-row")
          popSibling("table-row-group")
          popSibling("table-header-group")
          popSibling("table-footer-group")
          popSibling("table")
        }
      }
      parentStack.top.appendChild(child)
    }

    def add(child: BoxTreeNode) = {
      child match {
        case ab: AnonBox => {
          if (ab.isJustSpace) {
            if (!parentStack.top.displayInner.startsWith("table")) {
              parentStack.top.appendChild(ab)
            }
          } else {
            if (parentStack.top.displayInner.startsWith("table")) {
              needParent(0, List("table-cell"))
            }
            parentStack.top.appendChild(ab)
          }
        }
        case btnp: BoxTreeNodeProps => addWithChecks(btnp)
      }
    }

    origDomChildren foreach {child => add(child)}

    // unwind
    while (parentStack.nonEmpty) {
      val popped = parentStack.pop()
      if (parentStack.nonEmpty) {
        // println(s"appending $popped to ${parentStack.top}")
        parentStack.top.appendChild(popped)
      }
    }

    boxyDomChildren.foreach {case bwp: BoxTreeNodeProps => bwp.adjustChildren(); case _ => }


  }

  val tag = elemNode.elem.tag

  val img:BufferedImage = if (tag == "img") {
    val src = elemNode.elem.getAttribute("src")
    val baseUrl = elemNode.elem.getBaseURI()
    val imgUrl = new URL(new URL(baseUrl), src)
    ImageIO.read(imgUrl)
  } else {
    null
  }


  val id = elemNode.elem.getAttribute("id")
  val classAttrib = elemNode.elem.getAttribute("class")
  val debugId = "<" + tag + Option(id).map("#"+_).getOrElse("") + Option(classAttrib).map("."+_).getOrElse("") +">"

  val isRootElem = elemNode.elem.isRootElem
  val positionProp = elemNode.positionProp.get

  val isReplaced = (tag == "img") // && (b.img != null)

  val displayComputed = elemNode.displayProp.computed.get
  val displayOuter = BoxUtil.displayOuterMap.getOrElse(displayComputed, "block")
  val displayInner = BoxUtil.displayInnerMap.getOrElse(displayComputed, displayComputed)

  private lazy val createsBFC = {
    val floatProp = elemNode.floatProp.get
    floatProp == "left" || floatProp == "right" || positionProp == "absolute" || positionProp == "fixed" || displayInner == "flow-root" || displayInner == "table-cell"
    // TODO: Add more conditions
  }

  private def getReplacedWidth:LengthSpec = {
    if (img != null) {
      size.width.specified
    } else NoneLength
  }

  private def getReplacedHeight:LengthSpec = {
    if (img != null) {
      size.height.specified
    } else NoneLength
  }

  // formatting context established by this box
  private val formattingContext: Option[FormattingContext] = if (isReplaced) {
    Some(new SimpleReplacedFormattingContext(img))
  } else if (isRootElem || createsBFC) {
    Some(new FlowFormattingContext)
  } else {
    None
  }

  private val applicableFormattingContext: FormattingContext = formattingContext.getOrElse(domParentBox.map(_.applicableFormattingContext).get)

  // Level2 Properties
  private val backgroundProps = new BackgroundProps()
  val colorProp = new ColorProp("color")
  val fontProp = new FontProp()

  // Level 3 properties
  private val size = new SizeProps()
  private val border: Sides[Border] = new Sides[Border](new Border)

  private def computeBorderProps(vwProps: ViewPortProps) = {
    val nd = elemNode.nd
    val currColor = colorProp.computed
    border.forEach{(name, side) =>
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

  private var overflowX = "visible"
  private var overflowY = "visible"
  var visibility = true
  private var textAlignOpt:Option[String] = None

  private def computeSelfL2Props(vwProps: ViewPortProps) = {
    val nd = elemNode.nd
    val parentColor = domParentBox.map(_.colorProp.computed).getOrElse(Color.BLACK)
    // println(debugId + ": " + parentColor)
    colorProp.init(nd, parentColor)
    fontProp.init(nd, domParentBox.map(_.fontProp), vwProps)
    computeBorderProps(vwProps)
    backgroundProps.init(nd, fontProp)
    visibility = parseVisibility()
    overflowX = Property.getSpec(nd, "overflow-x").getOrElse("visible")
    overflowY = Property.getSpec(nd, "overflow-y").getOrElse("visible")
    textAlignOpt = Property.getSpec(nd, "text-align").orElse(domParentBox.flatMap(_.textAlignOpt))

    size.init(nd)
  }

  private def parseVisibility() = {
    val specified = Property.getSpec(elemNode.nd, "visibility")
    specified map {
      case "hidden" => false
      case "collapse" => false
      case _ => true
    } getOrElse (domParentBox.map(_.visibility).getOrElse(true))
  }

  def computeL2Props(vwProps: ViewPortProps):Unit = {
    computeSelfL2Props(vwProps)
    boxyDomChildren.foreach(_.computeL2Props(vwProps))
  }

  private val blockLevel = displayOuter == "block"

  def dump(level: Int): String = {
    ("  " * level) + s"$debugId\n" + boxyDomChildren.map(_.dump(level + 1)).mkString("\n")
  }

  private def computePaddings(paddingThickness: Sides[LengthSpec]) = {
    paddingThickness.left = size.paddingLeft.specified
    paddingThickness.right = size.paddingRight.specified
    paddingThickness.top = size.paddingTop.specified
    paddingThickness.bottom = size.paddingBottom.specified
  }

  override def toString = debugId

  def getContents(aParent: Content, vwProps: ViewPortProps): Vector[Content] = {
    val paddingThickness = new Sides[LengthSpec](NoneLength)
    computePaddings(paddingThickness )

    val marginSpecified = new Sides[LengthSpec](NoneLength);
    marginSpecified.top = size.marginTop.specified
    marginSpecified.bottom = size.marginBottom.specified
    marginSpecified.left = size.marginLeft.specified
    marginSpecified.right = size.marginRight.specified
    val widthSpecified = size.width.specified
    val heightSpecified = size.height.specified

    val offsets = new Sides[LengthSpec](NoneLength);
    def getOffset(name: String):LengthSpec = Property.getSpec(elemNode.nd, name).map(LengthProp.parseSpec(_)).getOrElse(AutoLength)
    offsets.top = getOffset("top")
    offsets.right = getOffset("right")
    offsets.bottom = getOffset("bottom")
    offsets.left = getOffset("left")

    val compMinWidth = size.minWidth.specified
    val compMaxWidth = size.maxWidth.specified

    // TODO: Default should be "right" when direction is "rtl"
    val textAlign = textAlignOpt.getOrElse("left")

    val renderPropsComputed = new RenderProps(backgroundProps, overflowX, overflowY, visibility)

    if (isReplaced) {
      if (displayOuter == "block") {
        if (tag == "img") {
          val painter: CanPaint = (g: Graphics2D, box: Box) => {
            g.drawImage(img, 0, 0, box.contentWidth, box.contentHeight, null)
          }
          Vector(new BlockContent(aParent, Some(painter), "blck replaced " + debugId, renderPropsComputed) {
              def getFormattingContext() = applicableFormattingContext
              def getSubContent() = Vector.empty
              val props = new LayoutProps(
                displayOuter, displayInner, positionProp,
                marginSpecified, border, paddingThickness,
                getReplacedWidth, compMinWidth, compMaxWidth,
                getReplacedHeight, fontProp, offsets)
              override def toString = "blk cntnt wrpr for " + debugId
            }
          )
        } else {
          // TODO
          println("TODO: " + debugId)
          ???
        }
      } else {
        Vector(new InlineRenderable {
          val parent = aParent
          override def toString = "inline replaced " + debugId
          def paintSelf(g: Graphics2D): Unit = {
            g.drawImage(img, 0, 0, box.contentWidth, box.contentHeight, null)
          }
          override def getFormattingContext():FormattingContext = applicableFormattingContext
          val props = new LayoutProps(
            "inline", "flow", positionProp,
            new Sides[LengthSpec](NoneLength), ContentUtil.emptyBorder, paddingThickness,
            getReplacedWidth, compMinWidth, compMaxWidth,
            getReplacedHeight, fontProp, offsets)
          val renderProps: RenderProps = renderPropsComputed
        })
      }
    } else {
      val compProps = new LayoutProps(
        displayOuter, displayInner, positionProp,
        marginSpecified, border, paddingThickness,
        widthSpecified, compMinWidth, compMaxWidth,
        heightSpecified, fontProp, offsets,
        textAlign)
      if (displayInner == "table") {
        Vector(new TableContent(debugId, compProps, renderPropsComputed, aParent, boxyDomChildren, vwProps))
      } else {
        if (displayOuter == "block") {
          Vector(new BlockContent(aParent, None, debugId, renderPropsComputed) {
                def getFormattingContext() = applicableFormattingContext
                def getSubContent() = boxyDomChildren.flatMap(_.getContents(this, vwProps))
                override def toString = "blk cntnt wrpr for " + debugId
                val props = compProps
              }
          )
        } else {
          if (tag == "br") {
            Vector(new InlineBreak(aParent))
          } else {
            if (displayInner != "flow") {
              Vector(new StandardInlineRenderable(aParent, debugId, applicableFormattingContext, compProps, renderPropsComputed, boxyDomChildren, vwProps))
            } else {
              boxyDomChildren.flatMap(_.getContents(aParent, vwProps))
            }
          }
        }
      }
    }
  }
}

class StandardInlineRenderable(val parent: Content, debugId: String, fc: FormattingContext, val props: LayoutProps, val renderProps: RenderProps, domChildren: Vector[BoxTreeNode], vwProps: ViewPortProps) extends InlineRenderable {
  override def toString = "inline wrapper for " + debugId
  def paintSelf(g: Graphics2D): Unit = { }
  override def getFormattingContext() = fc
  override def getSubContent() = domChildren.flatMap(_.getContents(this, vwProps))
}

sealed trait ContainingAreaType
case object WholeArea extends ContainingAreaType
case object PaddingArea extends ContainingAreaType
case object ContentArea extends ContainingAreaType

object BoxUtil {
  val tableCellContainers = List("table", "table-row-group", "table-header-group", "table-footer-group", "table-row")

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
