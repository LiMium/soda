package soda.poc

import soda.utils.Util
import java.awt.Graphics2D
import soda.layout.ViewPortProps
import java.awt.Color

sealed trait Queued {
  protected var queue: Vector[InlineRenderable] = Vector.empty

  protected def appendToQueue(ir: InlineRenderable) = {
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
  protected def addImpl(ir: InlineRenderable): Unit
}

class Line(val yPos: Int, val maxWidth: Int, vwProps: ViewPortProps) extends Queued {
  private var renderables = Vector[InlineRenderable]()
  private var height = 0
  private var width = 0

  def willFit(ir: InlineRenderable) = {
    val requiredSpace = ir.box.marginBoxWidth + queue.map(_.box.marginBoxWidth).sum
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
    if (ir.isSpace || ir.isBreak) {
      appendToQueue(ir)
    } else {
      flushQueue()
      addImpl(ir)
    }
  }

  protected def addImpl(ir: InlineRenderable): Unit = {
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
    c match {
      case ir: InlineRenderable =>
        if (ir.isSpace || ir.isBreak) {
          appendToQueue(ir)
        } else {
          flushQueue()
          addImpl(ir)
        }
      case bc: BlockContent => Util.warnln("Unexpected block content in inline mini context"); ???
    }
  }

  protected def addImpl(ir: InlineRenderable) = {
    Util.logLayout(2, s"adding ir $ir est dim ${ir.props.width}x${ir.props.height}", ir.level)

    if (ir.isBreak) {
      startNewLine()
    } else {
      if (ir.getFormattingContext() != null) {
        if (currLine == null) {
          startNewLine()
        }
        val newLC = lc.copy(widthConstraint = newWc)
        ir.getFormattingContext().innerLayout(ir, newLC)
      } else {
        val iWidth = ir.resolveLength(ir.props.width, maxLineWidth, autoValue = Some(maxLineWidth), noneValue = None).getOrElse(0)
        ir.box.contentWidth = iWidth

        val iHeightOpt = ir.resolveLength(ir.props.height, 0, autoValue = None, noneValue = Some(0))
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
}

class BlockMiniContext(c: Content, fc: FormattingContext, lc: LayoutConstraints) extends MiniContext[Content] {
  private var blocks: Vector[Content] = Vector.empty
  private var currY = 0
  override def add(bc: Content): Unit = {
    addImpl(bc, true)
  }

  private def addImpl(bc: Content, innerLayoutNeeded: Boolean) = {
    bc.box.offsetX = 0
    bc.box.offsetY = currY
    bc.computeRelativeOffsets(lc.vwProps)
    Util.logLayout(3, s"added block at ${bc.box.offsetX}, ${bc.box.offsetY}", bc.level)

    if (innerLayoutNeeded) {
      bc.getFormattingContext().innerLayout(bc, lc)
    }
    currY += bc.box.marginBoxHeight
    blocks :+= bc
  }

  def add(ctx: MiniContext[Content]): Unit = {
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

  def paint(g: Graphics2D): Unit = {
    // println("Block mini context paint")
    blocks.foreach(_.paintAll(g))
  }

  def getHeight: Int =  currY

  def getCurrPosXY(): (Int, Int) = {
    (0, currY)
  }
}

final class FlowFormattingContext(estBox: BoxWithProps) extends FormattingContext {
  private def marginTranslate(c: Content, m: LengthSpec): Option[Int] = {
    c.resolveLength(m, 0, None, Some(0))
  }


  private def natural(i: Int) = if (i < 0) 0 else i

  /**
    * compute widths and margins as per CSS2.2 section 10.3.3: Block-level non replaced elements
    *
    * @param widthOpt
    * @param avlWidth   :  Here available width is expected to be totalAvlWidth - (borderWidth + paddingWidth)
    * @param borderPaddingWidth
    * @param compMarginLeft
    * @param compMarginRight
    */
  private def computeWidthMargins(widthOpt: Option[Int], avlWidth: Int, compMarginLeft: Option[Int], compMarginRight: Option[Int]) = {
    widthOpt match {
      case None =>
          val mLeft = compMarginLeft.getOrElse(0)
          val mRight = compMarginRight.getOrElse(0)
          val rem = avlWidth - (mLeft + mRight)
          (rem, mLeft, mRight)
      case Some(width) =>
        if (compMarginLeft.isDefined) {
          // overconstrained, marginRight has to absorb the difference
          val rem = natural(avlWidth - (width + compMarginLeft.get))
          (width, compMarginLeft.get, rem)
        } else if (compMarginRight.isDefined) {
          val rem = natural(avlWidth - (width + compMarginRight.get))
          (width, rem, compMarginRight.get)
        } else {
          val rem = natural(avlWidth - width)
          val remBy2 = rem/2
          (width, remBy2, rem - remBy2)
        }
    }
  }

  def constrainWidth(tentativeWidth: Int, compMinWidth: Int, compMaxWidth: Option[Int]) = {
    val widthMinChecked = math.max(compMinWidth, tentativeWidth)
    val widthMaxChecked = compMaxWidth.map(math.min(_, widthMinChecked)).getOrElse(widthMinChecked)
    (widthMaxChecked, widthMaxChecked != tentativeWidth)
  }

  def innerLayout(c: Content, lc: LayoutConstraints) = {
    Util.logLayout(1, s"inner layout of $c", c.level)

    c.box.border = c.props.border
    c.box.paddingThickness = c.computePaddings()
    Util.logLayout(4, "padding " + c.box.paddingThickness, c.level)

    c.box.marginThickness.top = marginTranslate(c, c.props.margin.top).getOrElse(0)
    c.box.marginThickness.bottom = marginTranslate(c, c.props.margin.bottom).getOrElse(0)

    // TODO: The below subtraction could be done by the caller of innerLayout
    val avlWidth = lc.widthConstraint.avl - c.box.marginBoxSansContentWidth

    val containingWidth = c.containingWidth
    val cWidth = c.resolveLength(c.props.width, containingWidth, None, None)

    val cMarginLeft = marginTranslate(c, c.props.margin.left)
    val cMarginRight = marginTranslate(c, c.props.margin.right)

    val isShrinkToFit = lc.widthConstraint match {
      case FitAvailable(avl) => false
      case FitToShrink(avl) => true
    }

    val (tentativeWidth, marginLeft, marginRight) = if (!isShrinkToFit) {
      computeWidthMargins(cWidth, avlWidth, cMarginLeft, cMarginRight)
    } else {
      val mLeft = cMarginLeft.getOrElse(0)
      val mRight = cMarginRight.getOrElse(0)
      val actualAvlWidth = avlWidth - (mLeft + mRight + c.box.paddingWidth + c.box.borderWidth)
      val pw = preferredWidths(c)
      val shrinkToFitWidth = math.min(math.max(pw.prefMinWidth, actualAvlWidth), pw.prefWidth)
      (shrinkToFitWidth, mLeft, mRight)
    }

    c.box.marginThickness.left = marginLeft
    c.box.marginThickness.right = marginRight

    // println(c)
    // println(s"  Tentative width: $tentativeWidth, minWidth: ${c.props.compMinWidth} maxWidth: ${c.props.compMaxWidth}")
    val minWidth = c.resolveLength(c.props.compMinWidth, containingWidth).getOrElse(0)
    val maxWidth = c.resolveLength(c.props.compMaxWidth, containingWidth, autoValue = None, None)
    val (width, _) = constrainWidth(tentativeWidth, minWidth, maxWidth)
    // println(s"  width: $width")

    c.box.contentWidth = width

    val newWc = lc.widthConstraint match {
      case FitAvailable(avl) => FitAvailable(width)
      case FitToShrink(avl) => FitToShrink(width)
    }
    val newLC = lc.copy(widthConstraint = newWc)

    val heightDefinedOpt = c.resolveLength(c.props.height, if (c.parent != null) c.containingHeight else 0, autoValue = None, noneValue = None)
    heightDefinedOpt foreach {hd => c.box.contentHeight = hd}

    var contents = c.getSubContent()
    var currIMC:InlineMiniContext = null
    var currBMC:BlockMiniContext = null
    val textAlign = c.props.textAlign

    def initCurrIMC() = {if (currIMC == null) currIMC = new InlineMiniContext(c.level+1, textAlign, newLC) }
    def initCurrBMC() = {
      if (currBMC == null) {
        currBMC = new BlockMiniContext(c, this, newLC)
      }
    }
    contents foreach {ch =>
      Util.logLayout(2, "considering " + ch, ch.level)
      val mc = ch match {
        case ir: InlineRenderable =>
          initCurrIMC()
          currIMC
        case bc: BlockContent =>
          // println("  block content", bc)
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

    // TODO: Move this to base class
    c.absolutes.foreach {abs =>
      val cWidth = abs.containingWidth
      val cHeight = abs.containingHeight

      val absLC = new LayoutConstraints(FitToShrink(cWidth), FitToShrink(cHeight), lc.vwProps)
      abs.getFormattingContext().innerLayout(abs, absLC)

      def resolveAbsLength(s: LengthSpec, cl: Int) = { abs.resolveLength(s, cl, autoValue = None, noneValue = None) }

      val topOpt = resolveAbsLength(abs.props.offsets.top, cHeight)
      val bottomOpt = resolveAbsLength(abs.props.offsets.bottom, cHeight)
      val leftOpt = resolveAbsLength(abs.props.offsets.left, cWidth)
      val rightOpt = resolveAbsLength(abs.props.offsets.right, cWidth)

      if (topOpt.isDefined) {
        abs.box.offsetY = topOpt.get
      } else if (bottomOpt.isDefined) {
        abs.box.offsetY = cHeight - (abs.box.marginBoxHeight + bottomOpt.get)
      }

      if (rightOpt.isDefined) {
        abs.box.offsetX = cWidth - (abs.box.marginBoxWidth + rightOpt.get)
      } else if (leftOpt.isDefined) {
        abs.box.offsetX = leftOpt.get
      }
    }
    Util.logLayout(1, s"done inner layout of $c, dim: ${c.box.marginBoxWidth} x ${c.box.marginBoxHeight}", c.level)
  }

  def preferredWidths(c: Content): PrefWidths = {
    val rwOpt = c.resolveLength(c.props.width, c.containingWidth, autoValue = None, noneValue = None)
    val pwResult = rwOpt.map(rw => PrefWidths(rw, rw)).getOrElse({
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
        sc match {
          case ir: InlineRenderable =>
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
                irFC.preferredWidths(ir)
              }
              prefWidth += pw.prefWidth
              if (pw.prefMinWidth > prefMinWidth) {
                prefMinWidth = pw.prefMinWidth
              }
            }
          case bc: BlockContent =>
            endLine()
            val pw = bc.getFormattingContext().preferredWidths(bc)
            if (pw.prefWidth > prefWidth) {
              prefWidth = pw.prefWidth
            }
            if (pw.prefMinWidth > prefMinWidth) {
              prefMinWidth = pw.prefMinWidth
            }
        }
      })
      endLine()

      PrefWidths(prefMinWidth, prefWidth)
    })
    pwResult
  }
}
