package soda.layout

import java.awt.Graphics2D
import java.awt.Color
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps
import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.ImageIO

trait CanPaint {
  def paint(g: Graphics2D, box: Box): Unit
}

sealed trait BoxTreeNode {
  def getContents(parent: Content, vwProps: ViewPortProps): Vector[Content]

  def initProps(vwProps: ViewPortProps):Unit

  var boxyDomChildren : Vector[BoxTreeNode] = Vector.empty

  def dump(level: Int): String

  def computeL2Props(vwProps: ViewPortProps):Unit
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
    words.map(w => new InlineWordRenderable(parent, w, creator.visibility, creator.colorProp, creator.fontProp)).toVector
  }

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

  def dump(level: Int): String = {
    ("  " * level) + "Anon:\n" + tn
  }
  def computeL2Props(vwProps: ViewPortProps) = {}
}

// Root box will have parentBlock == None
class BoxWithProps(
  val elemNode: ElementNode,
  val domParentBox: Option[BoxWithProps]
  ) extends BoxTreeNode {

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
    floatProp == "left" || floatProp == "right" || positionProp == "absolute" || positionProp == "fixed" || displayInner == "flow-root"
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
    backgroundProps.init(nd)
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

  def initProps(vwProps: ViewPortProps):Unit = {
    computeL2Props(vwProps)
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
          val isBreak: Boolean = false
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
      if (displayOuter == "block") {
        Vector(new BlockContent(aParent, None, debugId, renderPropsComputed) {
              def getFormattingContext() = applicableFormattingContext
              def getSubContent() = boxyDomChildren.flatMap(_.getContents(this, vwProps))
              val props = new LayoutProps(
                displayOuter, displayInner, positionProp,
                marginSpecified, border, paddingThickness,
                widthSpecified, compMinWidth, compMaxWidth,
                heightSpecified, fontProp, offsets,
                textAlign)
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
              def paintSelf(g: Graphics2D): Unit = { }
              val isBreak: Boolean = false
              override def getFormattingContext() = applicableFormattingContext
              override def getSubContent() = boxyDomChildren.flatMap(_.getContents(this, vwProps))

              val props = new LayoutProps(
                "inline", displayInner, positionProp,
                marginSpecified, border, paddingThickness,
                widthSpecified, compMinWidth, compMaxWidth,
                heightSpecified, fontProp, offsets)

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

object BoxUtil {

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
