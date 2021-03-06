package soda.layout

import soda.utils.Util
import java.awt.Graphics2D
import soda.layout.ViewPortProps
import java.awt.Color

sealed trait Queued {
  protected var queue: Vector[Content] = Vector.empty

  protected def appendToQueue(ir: Content) = {
    queue :+= ir
  }

  protected def flushQueue() = {
    while (queue.nonEmpty) {
      addImpl(queue.head)
      queue = queue.drop(1)
    }
  }

  /**
    * This method should actually add the ir, and hence, not call appendToQueue()
    *
    * @param ir
    */
  protected def addImpl(ir: Content): Unit
}

class Line(val yPos: Int, val maxWidth: Int, vwProps: ViewPortProps) extends Queued {
  private var renderables = Vector[Content]()
  private var height = 0
  private var width = 0

  def isNotEmpty = renderables.nonEmpty

  def willFit(ir: Content) = {
    val requiredSpace = ir.box.marginBoxWidth + queue.map(_.box.marginBoxWidth).sum
    (requiredSpace + width) <= maxWidth
  }

  private def shouldIgnore(ir: Content) = {
    if (ir.isSpace) {
      renderables.lastOption.map(_.isSpace).getOrElse(true)
    } else {
      false
    }
  }

  def add(ir: Content) = {
    if (ir.isSpace || ir.isBreak) {
      appendToQueue(ir)
    } else {
      flushQueue()
      addImpl(ir)
    }
  }

  protected def addImpl(ir: Content): Unit = {
    if (!shouldIgnore(ir)) {
      ir.box.offsetX = width

      // TODO: Compute offset based on vertical alignment
      ir.box.offsetY = 0 // Note: offset of the ir is relative to line's position.

      ir.computeRelativeOffsets(vwProps)

      ir.box.paddingThickness = ir.computePaddings()

      height = math.max(height, ir.box.marginBoxHeight)
      width += ir.box.marginBoxWidth
      if (config.layoutDebugLevel > 3) println(s"    ir pos (${ir.box.offsetX},${ir.box.offsetY})  dim ${ir.box.marginBoxWidth} x ${ir.box.marginBoxHeight}")
      renderables = renderables.appended(ir)
    }
  }

  def getCurrPosX = width
  def getCurrPosY = height

  def remWidth = maxWidth - width

  def paintAll(g: Graphics2D) = {
    renderables.foreach{ir =>
      ir.paintAll(g)
    }
  }

  def completeLine(textAlign: String) = {
    if (textAlign == "right") {
      val rw = remWidth
      renderables.foreach{ir =>
        ir.box.offsetX += rw
      }
    }
  }
}

class InlineMiniContext(level: Int, textAlign: String, lc: LayoutConstraints) extends MiniContext[Content] with Queued {
  private val maxLineWidth = lc.widthConstraint.avl
  private val newWc = FitToShrink(maxLineWidth)

  override def add(c: Content): Unit = {
    c.props.displayOuter match {
      case "inline" =>
        val ir = c
        if (ir.isSpace || ir.isBreak) {
          appendToQueue(ir)
        } else {
          flushQueue()
          addImpl(ir)
        }
      case x => Util.warnln("Unexpected non-inline content in inline mini context: " + x);
    }
  }

  protected def addImpl(ir: Content) = {
    Util.logLayout(2, s"adding ir $ir est dim ${ir.props.width}x${ir.props.height}", ir.level)

    if (ir.isBreak) {
      startNewLine()
    } else {
      if (ir.getFormattingContext() != null) {
        if (currLine == null) {
          startNewLine()
        }
        val newLC = lc.copy(widthConstraint = newWc)
        ir.getFormattingContext().innerLayout(ir, 0, newLC)
      } else {
        val iWidth = ir.resolveLength(ir.props.width, Some(maxLineWidth), autoValue = Some(maxLineWidth), noneValue = None).getOrElse(0)
        ir.box.contentWidth = iWidth

        val iHeightOpt = ir.resolveLength(ir.props.height, None, autoValue = None, noneValue = Some(0))
        iHeightOpt.foreach {iHeight =>
          ir.box.contentHeight = iHeight
        }
      }
      if (currLine == null) {
        startNewLine()
      } else if (currLine.getCurrPosX != 0 && !currLine.willFit(ir)) {
        startNewLine()
      }
      currLine.add(ir)
    }
  }

  private var lines = Vector[Line]()
  private var currLine:Line = null
  private var currPosY = 0

  def getCurrPosXY(): (Int, Int) = {
    if (currLine == null) {
      (0, 0)
    } else {
      (currLine.getCurrPosX, currPosY)
    }
  }

  private def startNewLine() = {
    if (currLine != null) {
      currPosY += currLine.getCurrPosY
      currLine.completeLine(textAlign)
    }
    if (config.layoutDebugLevel > 2) println()
    Util.logLayout(2, "starting new line at " + currPosY, level)
    currLine = new Line(currPosY, maxLineWidth, lc.vwProps)
    lines :+= currLine
  }

  def paint(g: Graphics2D): Unit = {
    // println("Painting inline mini context")
    lines.foreach {l =>
      // val g2 = g.create(0, l.yPos, l.width, l.height).asInstanceOf[Graphics2D]
      val g2 = g.create.asInstanceOf[Graphics2D]
      g2.translate(0, l.yPos)
      if (config.paintDebugLevel > 2) {
        g.setColor(config.lineDebugColor)
        g.fillRect(0, l.yPos, l.getCurrPosX, l.getCurrPosY)
      }
      l.paintAll(g2)
      g2.dispose()
    }
  }

  def completeLayout() = {
    if (currLine != null) {
      currLine.completeLine(textAlign)
    }
  }
  def getHeight = lines.map(_.getCurrPosY).sum

  def isNotEmpty = lines.headOption.map(_.isNotEmpty).getOrElse(false)
}

class BlockMiniContext(c: Content, fc: FormattingContext, parentMarginCollapseTopAvl: Int, lc: LayoutConstraints) extends MiniContext[Content] {
  private var blocks: Vector[Content] = Vector.empty
  private var currY = 0
  override def add(bc: Content): Unit = {
    addImpl(bc, true)
  }

  private def findPrevNonEmpty(): Option[Content] = {
    blocks.reverseIterator.find(_.box.marginBoxHeight != 0)
  }

  private def computeMarginCollapseTopAvl(bc: Content): Int = {
    // blocks.lastOption.map(_.box.marginThickness.bottom).getOrElse(parentMarginCollapseTopAvl)
    findPrevNonEmpty.map(_.box.marginThickness.bottom).getOrElse(parentMarginCollapseTopAvl)
  }

  private def addImpl(bc: Content, innerLayoutNeeded: Boolean) = {
    bc.box.offsetX = 0
    bc.box.offsetY = currY

    val marginCollapseTopAvl = computeMarginCollapseTopAvl(bc)

    bc.computeRelativeOffsets(lc.vwProps)
    Util.logLayout(3, s"added block at ${bc.box.offsetX}, ${bc.box.offsetY}", bc.level)

    if (innerLayoutNeeded) {
      val vertAdvance = bc.getFormattingContext().innerLayout(bc, marginCollapseTopAvl, lc)
      currY += vertAdvance
    } else {
      currY += bc.box.marginBoxHeight
    }
    blocks :+= bc
  }

  def add(ctx: MiniContext[Content]): Unit = {
    if (ctx.isNotEmpty) {
      val bc = new BlockContent(c, None, "anon block wrapper for inline mini context", new RenderProps(null, "visible", "visible", true)) {
        val props = new LayoutProps(
          "block", "flow", "static",
          new Sides[LengthSpec](NoneLength), ContentUtil.emptyBorder, ContentUtil.emptyOffsets,
          AutoLength, ContentUtil.zeroLength, NoneLength,
          NoneLength, ContentUtil.emptyFontProp, ContentUtil.emptyOffsets)
        def getFormattingContext(): FormattingContext = fc
        def getSubContent(): Vector[Content] = Vector.empty
      }
      bc.miniContext = ctx
      addImpl(bc, false)
      currY += bc.miniContext.getHeight
    }
  }

  def paint(g: Graphics2D): Unit = {
    // println("Block mini context paint")
    blocks.foreach(_.paintAll(g))
  }

  def getHeight: Int =  currY

  def getCurrPosXY(): (Int, Int) = {
    (0, currY)
  }

  def isNotEmpty = blocks.nonEmpty
}

final class FlowFormattingContext extends FormattingContext {
  import FCUtil._

  def innerLayout(c: Content, marginCollapseTopAvl: Int, lc: LayoutConstraints) = {
    Util.logLayout(1, s"  flow inner layout of $c", c.level)

    c.hydrateSimpleProps()

    val width = finaliseWidthMargins(c, lc)

    val marginCollapseOffset = math.min(c.box.marginThickness.top, marginCollapseTopAvl)
    if (marginCollapseOffset > 0) {
      c.box.offsetY -= marginCollapseOffset
    }

    // println(s"  width: $width")

    val heightDefinedOpt = c.resolveLength(c.props.height, if (c.parent != null) Some(c.containingHeight) else None, autoValue = None, noneValue = None)
    heightDefinedOpt foreach {hd => c.box.contentHeight = hd}

    val newWc = lc.widthConstraint match {
      case FitAvailable(avl) => FitAvailable(width)
      case FitToShrink(avl) => FitToShrink(width)
    }
    val newLC = lc.copy(widthConstraint = newWc)

    var contents = c.getSubContent()
    var currIMC:InlineMiniContext = null
    var currBMC:BlockMiniContext = null
    val textAlign = c.props.textAlign
    val parentMarginCollapseTopAvl = if (c.box.border.top.thickness == 0 && c.box.paddingThickness.top == 0) {
      math.max(c.box.marginThickness.top, marginCollapseTopAvl)
    } else 0

    def initCurrIMC() = {if (currIMC == null) currIMC = new InlineMiniContext(c.level+1, textAlign, newLC) }
    def initCurrBMC() = {
      if (currBMC == null) {
        currBMC = new BlockMiniContext(c, this, parentMarginCollapseTopAvl, newLC)
      }
    }
    contents foreach {ch =>
      Util.logLayout(2, "considering " + ch, ch.level)
      val mc = ch.props.displayOuter match {
        case "inline" =>
          initCurrIMC()
          currIMC
        case "block" =>
          if (currIMC != null) {
            currIMC.completeLayout()
            initCurrBMC()
            currBMC.add(currIMC)
            currIMC = null
          }
          initCurrBMC()
          currBMC
      }
      if (ContentUtil.isAbsolutish(ch.props.position)) {
        val currPos = mc.getCurrPosXY()
        ContentUtil.transportAbs(ch, currPos)
      } else {
        mc.add(ch)
      }
    }
    if (currIMC != null) {
      currIMC.completeLayout()
      if (currBMC != null) {
        currBMC.add(currIMC)
        currIMC = null
      }
    }
    // update node's mini context
    c.miniContext = if (currIMC != null) currIMC else if (currBMC == null) EmptyMiniContext else currBMC
    if(heightDefinedOpt.isEmpty) {
      c.box.contentHeight = c.miniContext.getHeight
    }

    positionAbsolutes(c, lc.vwProps)

    Util.logLayout(1, s"✓ flow inner layout of $c, dim: ${c.box.marginBoxWidth} x ${c.box.marginBoxHeight}", c.level)

    c.box.marginBoxHeight - marginCollapseOffset
  }

  private def computeMarginPaddingBorderWidth(c: Content) : Int = {
    val ml = c.resolveLength(c.props.margin.left, None, None, None).getOrElse(0)
    val mr = c.resolveLength(c.props.margin.right, None, None, None).getOrElse(0)
    val bl = c.props.border.left.thickness
    val br = c.props.border.right.thickness
    val pl = c.resolveLength(c.props.padding.left, None, None, None).getOrElse(0)
    val pr = c.resolveLength(c.props.padding.right, None, None, None).getOrElse(0)

    ml + mr + pl + pr + bl + br
  }

  def preferredWidths(c: Content, withMarginPaddingBorder: Boolean): PrefWidths = {
    val mbpThickness = if (withMarginPaddingBorder) computeMarginPaddingBorderWidth(c) else 0
    val rwOpt = c.resolveLength(c.props.width, Some(c.containingWidth), autoValue = None, noneValue = None)
    val pwResult = rwOpt.map(rw => PrefWidths(rw + mbpThickness, rw + mbpThickness)).getOrElse({
      var prefMinWidth = 0
      var prefWidth = 0
      var currLinePrefWidth = 0

      def endLine() = {
        if (currLinePrefWidth > prefWidth) {
          prefWidth = currLinePrefWidth
        }
        currLinePrefWidth = 0
      }

      c.getSubContent().foreach(sc => {
        sc.props.displayOuter match {
          case "inline" =>
            val ir = sc
            if (ir.isBreak) {
              endLine()
            } else {
              val irFC = ir.getFormattingContext()
              val pw = if (irFC == null) {
                ir.props.width match {
                  case AbsLength(pixels) => PrefWidths(pixels.toInt, pixels.toInt)
                  case x => println(x); ???
                }
              } else {
                irFC.preferredWidths(ir, true)
              }
              prefWidth += pw.prefWidth
              if (pw.prefMinWidth > prefMinWidth) {
                prefMinWidth = pw.prefMinWidth
              }
            }
          case _ =>
            val bc = sc
            endLine()
            val pw = bc.getFormattingContext().preferredWidths(bc, true)
            if (pw.prefWidth > prefWidth) {
              prefWidth = pw.prefWidth
            }
            if (pw.prefMinWidth > prefMinWidth) {
              prefMinWidth = pw.prefMinWidth
            }
        }
      })
      endLine()

      PrefWidths(prefMinWidth + mbpThickness, prefWidth + mbpThickness)
    })
    pwResult
  }
}
