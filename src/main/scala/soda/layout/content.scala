package soda.layout

import java.awt.Graphics2D

import soda.layout.ViewPortProps
import java.awt.Color
import soda.utils.Util

/* Computed values */
class LayoutProps(
  val displayOuter: String,
  val displayInner: String,
  val position: String,
  val margin: Sides[LengthSpec],
  val border: Sides[Border],
  val padding: Sides[LengthSpec],
  val width: LengthSpec,
  val compMinWidth: LengthSpec,
  val compMaxWidth: LengthSpec,
  val height: LengthSpec,
  val fontProp: FontProp,
  val offsets: Sides[LengthSpec], // Used for top, left, bottom, right
  val textAlign : String = "left"
)

class RenderProps (
  val bgProps: BackgroundProps,
  val overflowX: String,
  val overflowY: String,
  val visibility: Boolean
)

case class ContainingBlockRef(areaType: ContainingAreaType, cb: Content) {
  def width = if (cb != null) {
     areaType match {
       case PaddingArea => cb.box.paddingBoxWidth
       case ContentArea => cb.box.contentWidth
       case WholeArea => ???
     }
    } else 0

  def height = if (cb != null) {
    areaType match {
      case PaddingArea => cb.box.paddingBoxHeight
      case ContentArea => cb.box.paddingBoxHeight
      case WholeArea => ???
    }
  } else 0

  def addAsAbsoluteChild(c: Content):Unit = {
    cb.absolutes :+= c
  }
}

sealed trait Content {
  val parent: Content
  lazy val level:Int = if (parent == null) 0 else parent.level + 1
  // input to layout
  val props: LayoutProps
  val renderProps: RenderProps
  def getFormattingContext(): FormattingContext
  def getSubContent(): Vector[Content]

  // output of layout
  val box = new Box()
  var miniContext: MiniContext[_] = null
  var absolutes: Vector[Content] = Vector.empty

  // after layout
  /**
    * Paint self within the content box
    *
    * @param g Origin is set at top-left of content box
    */
  protected def paintSelf(g: Graphics2D): Unit

  private def clip(g: Graphics2D):Unit = {
    val currClipBounds = g.getClipBounds
    val clipBoundWidth = if (renderProps.overflowX == "hidden" || renderProps.overflowX == "scroll") -currClipBounds.x + box.contentWidth else currClipBounds.width
    val clipBoundHeight = if (renderProps.overflowY == "hidden" || renderProps.overflowY == "scroll") -currClipBounds.y + box.contentHeight else currClipBounds.height
    g.clipRect(currClipBounds.x, currClipBounds.y, clipBoundWidth, clipBoundHeight)
  }

  def paintAll(g: Graphics2D): Unit = {
    val g2 = g.create().asInstanceOf[Graphics2D]
    g2.translate(box.paintOffsetX, box.paintOffsetY)
    if (renderProps.visibility) {
      box.paint(g2, renderProps.bgProps);
    }

    {
      val g3 = g2.create().asInstanceOf[Graphics2D]
      g3.translate(box.contentOffsetX, box.contentOffsetY)
      if (renderProps.visibility) {
        paintSelf(g3)
      }
      if (miniContext != null) {
        clip(g3)
        miniContext.paint(g3)
      }
      g3.dispose()
    }

    if (absolutes.length > 0) {
      val g3 = g2.create().asInstanceOf[Graphics2D]
      g3.translate(box.paddingBoxOffsetX, box.paddingBoxOffsetY)
      clip(g3)
      paintAbsolutes(g3)
      g3.dispose()
    }
    g2.dispose()
  }

  protected def paintAbsolutes(g: Graphics2D): Unit = {
    absolutes.foreach{ ab =>
      ab.paintAll(g)
    }
  }

  // util
  def resolveLength(lengthSpec: LengthSpec, containingBlockLength: Float): Option[Int] = {
    resolveLength(lengthSpec, Some(containingBlockLength), Some(containingBlockLength.toInt), Some(0))
  }

  def resolveLength(lengthSpec: LengthSpec, containingBlockLengthOpt: Option[Float], autoValue: Option[Int], noneValue: Option[Int]) = {
    lengthSpec match {
      case AbsLength(pxs) => Some(pxs.toInt)
      case frl: FontRelLength => Some(frl.compute(props.fontProp).toInt)
      case PercentLength(scale) => containingBlockLengthOpt.map(l => (l * scale ).toInt)
      case AutoLength => autoValue
      case NoneLength => noneValue
      case x => Util.warnln("Not handled: " + x); Some(0)
    }
  }
  private def findRelativeOffset(factor:Int, parentLength: Float, propName: String, vwProps: ViewPortProps) = {
    resolveLength(props.offsets.byName(propName), Some(parentLength), autoValue = None, noneValue = None).map(_ * factor)
  }

  private def findRootContentNode: Content = {
    if (parent.parent == null) {
      parent
    } else {
      parent.findRootContentNode
    }
  }

  private def findContainerForAbs: Content = {
    if (parent.props.position != "static") {
      parent
    } else {
      parent.findContainerForAbs
    }
  }

  lazy val containingBlock = {
    if (props.position == "absolute") {
      ContainingBlockRef(PaddingArea, findContainerForAbs)
    } else if (props.position == "fixed") {
      ContainingBlockRef(PaddingArea, findRootContentNode)
    } else {
      ContainingBlockRef(ContentArea, parent)
    }
  }

  def containingWidth = containingBlock.width
  def containingHeight = containingBlock.height

  def computeRelativeOffsets(vwProps: ViewPortProps) = {
    if (props.position == "relative") {
      box.renderOffsetY = findRelativeOffset(1, containingHeight, "top", vwProps).orElse(findRelativeOffset(-1, containingHeight, "bottom", vwProps)).getOrElse(0)
      box.renderOffsetX = findRelativeOffset(1, containingWidth, "left", vwProps).orElse(findRelativeOffset(-1, containingWidth, "right", vwProps)).getOrElse(0)
    }
  }

  def computePaddings(): SidesInt = {
    val paddingSpec = props.padding
    val result = new SidesInt()

    def resolve(spec: LengthSpec) = {
      resolveLength(spec, Some(containingWidth), autoValue=Some(0), noneValue = None).getOrElse(0)
    }

    // Note: percentage values for padding always refer to containing width and not height
    result.top = resolve(paddingSpec.top)
    result.right = resolve(paddingSpec.right)
    result.bottom = resolve(paddingSpec.bottom)
    result.left = resolve(paddingSpec.left)
    result
  }

  def hydrateSimpleProps(): Unit = {
    box.border = props.border
    box.paddingThickness = computePaddings()
  }
}

abstract class BlockContent(val parent: Content, canPaintOpt: Option[CanPaint], debugStr: String, val renderProps: RenderProps) extends Content {
  val displayOuter = "block"
  def paintSelf(g: Graphics2D): Unit = {
    canPaintOpt.foreach(cp => {
      cp.paint(g, box)
    })
    if (config.paintDebugLevel > 1) {
      g.setColor(Color.magenta.darker())
      g.drawRect(0, 0, box.contentWidth-1, box.contentHeight-1)
    }
  }
  override def toString(): String = debugStr
}

trait InlineRenderable extends Content {
  val isBreak: Boolean
  val isSpace: Boolean = false

  def getFormattingContext():FormattingContext = null
  def getSubContent(): Vector[Content] = Vector.empty
}

final class InlineBreak(val parent: Content) extends InlineRenderable {
  def paintSelf(g: Graphics2D): Unit = {}
  val isBreak: Boolean = true
  val props = new LayoutProps(
    "inline", "flow", "static",
    new Sides[LengthSpec](NoneLength), ContentUtil.emptyBorder, ContentUtil.emptyOffsets,
    NoneLength, ContentUtil.zeroLength, NoneLength,
    NoneLength,
    ContentUtil.emptyFontProp,
    ContentUtil.emptyOffsets)

  val renderProps: RenderProps = new RenderProps(null, "visible", "visible", true)
}

final class InlineWordRenderable(val parent: Content, word: String, visibility: Boolean, colorProp: ColorProp, fontProp: FontProp, vwProps: ViewPortProps) extends InlineRenderable {
  override def toString = s"word '$word' est: $estWidth x $estHeight"
  def paintSelf(g: Graphics2D): Unit = {
    if (visibility) {
      g.setColor(colorProp.computed)
      g.setFont(fontProp.font)
      // g.drawString(word, box.offsetX, box.offsetY + fontProp.ascent)
      g.drawString(word, 0, fontProp.ascent)
    }
  }

  private val measuredHeight = vwProps.getLineMetrics(fontProp.font, word).getHeight

  private val estWidth = AbsLength(fontProp.estWidth(word))
  private val estHeight = AbsLength(math.round(measuredHeight))

  val isBreak: Boolean = false
  override val isSpace: Boolean = word == " "
  val props = new LayoutProps(
    "inline", "flow", "static",
    new Sides[LengthSpec](NoneLength), ContentUtil.emptyBorder, ContentUtil.emptyOffsets,
    estWidth, ContentUtil.zeroLength, NoneLength,
    estHeight,
    fontProp,
    ContentUtil.emptyOffsets)

  val renderProps: RenderProps = new RenderProps(null, "visible", "visible", visibility)
}

object ContentUtil {
  val emptyBorder = new Sides[Border](new Border())
  val emptyOffsets = new Sides[LengthSpec](AutoLength)
  val emptySideInt = new SidesInt
  val emptyFontProp = new FontProp()
  val zeroLength = AbsLength(0)

  def isAbsolutish(position: String) = {
    absolutish.contains(position)
  }
  val absolutish = Array("absolute", "fixed")

  def findCascadingOffset(c: Content, containingBlock: ContainingBlockRef, pos: (Int, Int)): (Int, Int) = {
    if (c eq containingBlock.cb) {
      (pos._1 + c.box.paddingThickness.left, pos._2 + c.box.paddingThickness.top)
    } else {
      val pOffset = findCascadingOffset(c.parent, containingBlock, pos)
      (pOffset._1 + c.box.paintOffsetX + c.box.contentOffsetX, pOffset._2 + c.box.paintOffsetY + c.box.contentOffsetY)
    }
  }

  /**
    * Transport the given content to its containing block
    *
    * @param c
    * @param currPos
    */
  def transportAbs(c: Content, currPos: (Int, Int)): Unit = {
    val offset = findCascadingOffset(c, c.containingBlock, currPos)
    c.box.offsetX = offset._1
    c.box.offsetY = offset._2
    c.containingBlock.addAsAbsoluteChild(c)
  }
}

sealed trait LengthConstraint {
  val avl: Int
}
case class FitAvailable(avl: Int) extends LengthConstraint
case class FitToShrink(avl: Int) extends LengthConstraint

case class LayoutConstraints(
  widthConstraint: LengthConstraint,
  heightConstraint: LengthConstraint,
  vwProps: ViewPortProps)
