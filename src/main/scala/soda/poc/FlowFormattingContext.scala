package soda.poc

import soda.utils.Util
import java.awt.Graphics2D
import soda.layout.ViewPortProps
import java.awt.Color

sealed trait Queued {
  private var queue: Vector[InlineRenderable] = Vector.empty

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
  var renderables = Vector[InlineRenderable]()
  var height = 0
  var width = 0

  def willFit(ir: InlineRenderable) = {
    val requiredSpace = ir.props.width match {
      case AbsLength(pixels) => pixels.toInt
      case PercentLength(scale) => (scale * maxWidth).toInt
      case NoneLength => 0
      case AutoLength => maxWidth
      case x => println(x); ???
    }
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

      val iWidth = ir.props.width match {
        case AbsLength(pixels) => pixels.toInt
        case PercentLength(scale) => (scale * maxWidth).toInt
        case AutoLength => maxWidth
        case NoneLength => 0
        case x => soda.utils.Util.warnln("TODO: Inline width " + x); 0
      }
      val iHeightOpt = ir.props.height match {
        case AbsLength(pixels) => Some(pixels.toInt)
        case NoneLength => Some(0)
        case AutoLength => None
        case x => soda.utils.Util.warnln("TODO: Inline height " + x); Some(0)
      }
      ir.box.contentWidth = iWidth
      iHeightOpt.foreach {iHeight =>
        ir.box.contentHeight = iHeight
      }

      height = math.max(height, ir.box.marginBoxHeight)
      width += ir.box.marginBoxWidth
      if (config.layoutDebugLevel > 3) println(s"    ir pos (${ir.box.offsetX},${ir.box.offsetY})  dim $iWidth x ${ir.box.marginBoxHeight}")
      renderables = renderables.appended(ir)
    }
  }

  def getCurrPosX = width
}

class InlineMiniContext(level: Int, lc: LayoutConstraints) extends MiniContext[Content] with Queued {
  val maxLineWidth = lc.widthConstraint.avl

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
        val remWidth = maxLineWidth - currLine.width
        val newWc = lc.widthConstraint match {
          case FitAvailable(avl) => FitAvailable(remWidth)
          case FitToShrink(avl) => FitToShrink(remWidth)
        }
        val newLC = lc.copy(widthConstraint = newWc)
        ir.getFormattingContext().innerLayout(ir, newLC)
      }
      if (currLine == null) {
        startNewLine()
      } else if (currLine.width != 0 && !currLine.willFit(ir)) {
        startNewLine()
      }
      currLine.add(ir)
    }
  }

  var lines = Vector[Line]()
  var currLine:Line = null
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
      currPosY += currLine.height
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
        g.fillRect(0, l.yPos, l.width, l.height)
      }
      l.renderables.foreach{ir =>
        ir.paintAll(g2)
      }
      g2.dispose()
    }
  }

  def getHeight = lines.map(_.height).sum
  def getWidth = lines.map(_.width).maxOption.getOrElse(0)
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
        NoneLength, ContentUtil.emptyOffsets)
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
  private def marginTranslate(m: LengthSpec): Option[Int] = {
    m match {
      case AutoLength => None
      case NoneLength => Some(0)
      case AbsLength(pixels) => Some(pixels.toInt)
      case x => Util.warnln("Not handled " + x); Some(0)
    }
  }

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
    def natural(i: Int) = if (i < 0) 0 else i

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

  def resolveLength(l: LengthSpec, containerLength: Float) = {
    l match {
      case AbsLength(pixels) => Some(pixels.toInt)
      // case ParentRelLength(scale) =>  Some(if (c.parent != null) (c.parent.box.contentWidth*scale).toInt else 0)
      case PercentLength(scale) =>  Some((scale * containerLength).toInt)
      case _ => None
    }
  }

  def innerLayout(c: Content, lc: LayoutConstraints) = {
    Util.logLayout(1, s"inner layout of $c", c.level)

    c.box.border = c.props.border
    c.box.paddingThickness = c.computePaddings()
    Util.logLayout(4, "padding " + c.box.paddingThickness, c.level)

    c.box.marginThickness.top = marginTranslate(c.props.margin.top).getOrElse(0)
    c.box.marginThickness.bottom = marginTranslate(c.props.margin.bottom).getOrElse(0)

    // TODO: The below subtraction could be done by the caller of innerLayout
    val avlWidth = lc.widthConstraint.avl - c.box.marginBoxSansContentWidth

    /*
    val cWidth = c.props.width match {
      case AbsLength(pixels) => Some(pixels.toInt)
      case ParentRelLength(scale) =>  Some(if (c.parent != null) (c.parent.box.contentWidth*scale).toInt else 0)
      case _ => None
    }
    */
    val containingWidth = c.containingWidth
    val cWidth = resolveLength(c.props.width, containingWidth)

    val cMarginLeft = marginTranslate(c.props.margin.left)
    val cMarginRight = marginTranslate(c.props.margin.right)

    val (tentativeWidth, marginLeft, marginRight) = if (c.props.displayOuter == "block" && Array("static", "relative").contains(c.props.position)) {
      computeWidthMargins(cWidth, avlWidth, cMarginLeft, cMarginRight)
    } else {
      // TODO: This case needs a lot more work
      (cWidth.getOrElse(avlWidth), cMarginLeft.getOrElse(0), cMarginRight.getOrElse(0))
    }

    c.box.marginThickness.left = marginLeft
    c.box.marginThickness.right = marginRight

    // println(c)
    // println(s"  Tentative width: $tentativeWidth, minWidth: ${c.props.compMinWidth} maxWidth: ${c.props.compMaxWidth}")
    val minWidth = resolveLength(c.props.compMinWidth, containingWidth).getOrElse(0)
    val maxWidth = resolveLength(c.props.compMaxWidth, containingWidth)
    val (width, _) = constrainWidth(tentativeWidth, minWidth, maxWidth)
    // println(s"  width: $width")

    c.box.contentWidth = width

    val newWc = lc.widthConstraint match {
      case FitAvailable(avl) => FitAvailable(width)
      case FitToShrink(avl) => FitToShrink(width)
    }
    val newLC = lc.copy(widthConstraint = newWc)

    val heightDefinedOpt = c.props.height match {
      case AbsLength(pixels) => Some(pixels.toInt)
      case PercentLength(scale) => Some(if (c.parent != null) (c.containingHeight*scale).toInt else 0)
      case AutoLength => None
      case NoneLength => None
      case x => println("TODO height: " + x); None
    }
    heightDefinedOpt foreach {hd => c.box.contentHeight = hd}

    var contents = c.getSubContent()
    var currIMC:InlineMiniContext = null
    var currBMC:BlockMiniContext = null
    def initCurrIMC() = {if (currIMC == null) currIMC = new InlineMiniContext(c.level+1, newLC) }
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
    if (currIMC != null && currBMC != null) {
      currBMC.add(currIMC)
      currIMC = null
    }
    // update node's mini context
    c.miniContext = if (currIMC != null) currIMC else if (currBMC == null) EmptyMiniContext else currBMC
    if(heightDefinedOpt.isEmpty) {
      c.box.contentHeight = c.miniContext.getHeight
    }

    c.absolutes.foreach {abs =>
      val absLC = new LayoutConstraints(FitToShrink(abs.containingWidth), FitToShrink(abs.containingHeight), lc.vwProps)
      abs.getFormattingContext().innerLayout(abs, absLC)
    }
    Util.logLayout(1, s"inner layout of $c, dim: ${c.box.marginBoxWidth} x ${c.box.marginBoxHeight}", c.level)
  }
}
/*

final class FlowFormattingContext(estBox: BoxWithProps) extends FormattingContext {

  override def getFlowBoxType(displayOuter: String): InnerBoxType = {
    if (displayOuter == "inline" || displayOuter == "run-in") {
      InlineBoxType
    } else {
      BlockContainerBoxType
    }
  }


  private def inlineLayoutAIB(ab: AnonBox, vwProps: ViewPortProps): Unit = {
    ab.inlinePseudoContext.maxWidth = ab.creator.b.contentWidth
    getSimpleInlineRenderables(ab, vwProps).foreach(ab.inlinePseudoContext.addInlineRenderable)
    ab.b.contentHeight = ab.inlinePseudoContext.getHeight
    ab.b.contentWidth = ab.inlinePseudoContext.getWidth
  }


  private def doLayout(boxP: BoxWithProps, vwProps: ViewPortProps): Unit = {
    boxP.computePaddings(vwProps)

    // Find width of this block
    // TODO: revisit this quick hack for replaced elements
    if (boxP.isReplaced) {
      boxP.computeWidths()
    } else {

      def constrainWidth(tentativeWidth: Int) = {
        val widthMinChecked = math.max(boxP.getComputedMinWidth, tentativeWidth)
        val widthMaxChecked = boxP.getComputedMaxWidth.map(math.min(_, widthMinChecked)).getOrElse(widthMinChecked)
        (widthMaxChecked, widthMaxChecked != tentativeWidth)
      }

      val compWidth = boxP.getComputedWidth
      val avlWidth = boxP.containingWidth
      val compMarginLeft = boxP.getComputedMargin("left")
      val compMarginRight = boxP.getComputedMargin("right")
      val compMarginTop = boxP.getComputedMargin("top")
      val compMarginBottom = boxP.getComputedMargin("bottom")
      val borderPaddingWidth = boxP.b.borderPaddingWidth

      def computeWhenWidthSpecified(width: Int) = {
        if (compMarginLeft.isDefined) {
          // overconstrained, marginRight has to absorb the difference
          val rem = avlWidth - (width + borderPaddingWidth + compMarginLeft.get)
          (false, width, compMarginLeft.get, rem)
        } else if (compMarginRight.isDefined) {
          val rem = avlWidth - (width + borderPaddingWidth + compMarginRight.get)
          (false, width, rem, compMarginRight.get)
        } else {
          val rem = avlWidth - (width + borderPaddingWidth)
          val remBy2 = rem/2
          (false, width, remBy2, rem - remBy2)
        }
      }

      val (shrinkToFit, usedWidth, marginLeft, marginRight) = if (boxP.displayOuter == "inline" && boxP.displayInner == "flow-root") {
        // As per section 10.3.9 of CSS 2.2
        // TODO: width should be shrink to fit, when it has computed value of auto. But that will have to be handled during layout
        (compWidth.isEmpty, compWidth.getOrElse(avlWidth), compMarginLeft.getOrElse(0), compMarginRight.getOrElse(0))
      } else {
        compWidth match {
          case Some(absComputedWidth) => {
            val (width, _) = constrainWidth(absComputedWidth)
            computeWhenWidthSpecified(width)
          }
          case None => {
            // Width is auto
            val marginLeftElseZero = compMarginLeft.getOrElse(0)
            val marginRightElseZero = compMarginRight.getOrElse(0)
            val avlWidthModuloMargins = avlWidth - (marginLeftElseZero + marginRightElseZero + borderPaddingWidth)
            val (width, notAuto) = constrainWidth(avlWidthModuloMargins)
            if (notAuto) {
              computeWhenWidthSpecified(width)
            } else {
              (false, width, marginLeftElseZero, marginRightElseZero)
            }
          }
        }
      }
      boxP.b.contentWidth = usedWidth
      boxP.b.shrinkToFit = shrinkToFit
      boxP.b.marginThickness.left = marginLeft
      boxP.b.marginThickness.right = marginRight
      boxP.b.marginThickness.top = onlyPositive(compMarginTop.getOrElse(0))
      boxP.b.marginThickness.bottom = onlyPositive(compMarginBottom.getOrElse(0))
      // println(boxP.debugId + "  margin thickness: " + boxP.b.marginThickness)
      // println(boxP.debugId + "  padding thickness: " + boxP.b.paddingThickness)
      // println("  content width: " + usedWidth)
      // println("  border thickness: " + boxP.b.border)
    }

    // println("In : " + boxP.debugId)
    if (boxP.inlineMode) {
      if(config.layoutDebugLevel > 0) println(s"Starting inline layout of $boxP")
      val heightModified = boxP.computeHeights()
      inlineLayout(boxP, !heightModified, vwProps)
    } else {
      if(config.layoutDebugLevel > 0) println(s"Starting block layout of $boxP")
      blockLayout(boxP, vwProps)
    }
    absLayout(boxP, vwProps)
  }

  private def inlineLayout(boxP: BoxWithProps, heightUpdate: Boolean, vwProps: ViewPortProps): Unit = {
    // println("Doing inline layout in ", debugId, b.offsetY)
    val ipc = boxP.inlinePseudoContext
    val b = boxP.b

    ipc.maxWidth = b.contentWidth
    inlineLayoutRecurse(boxP, ipc, vwProps)
    if (heightUpdate) {
      b.contentHeight = ipc.getHeight
    }
    if (b.shrinkToFit) {
      b.contentWidth = math.min(ipc.getWidth, b.contentWidth)
    }
    // b.contentWidth = inlinePseudoContext.getWidth
  }

  private def inlineLayoutRecurse(btn: BoxTreeNode, ipc: InlineMiniContext, vwProps: ViewPortProps): Unit = {
    def add(ir: InlineRenderable) = ipc.addInlineRenderable(ir)
    btn match {
      case boxP: BoxWithProps =>
        def handleOutOfFlow(oboxP: BoxWithProps) = {
            // println("Setting offset of inline out of flow", boxP)
            val currPos = ipc.getCurrPosXY()
            // TODO: Combine these two calls into one
            val cascadingOffsetY = Util.findCascadingOffsetY(boxP, oboxP.containingBlock, currPos._2)
            val cascadingOffsetX = Util.findCascadingOffsetX(boxP, oboxP.containingBlock, currPos._1)
            oboxP.b.offsetY = cascadingOffsetY
            oboxP.b.offsetX = cascadingOffsetX
        }
        if (boxP.tag == "img") {
          if (boxP.b.img != null) {
            boxP.computePaddings(vwProps)
            if(!boxP.computeWidths()) {
              // use intrinsic
              boxP.b.contentWidth = boxP.b.img.getWidth
            }
            if(!boxP.computeHeights()) {
              // use intrinsic
              boxP.b.contentHeight = boxP.b.img.getHeight
            }
            add(new InlineElemRenderable(boxP.b))
          }
        } else if (boxP.tag == "br") {
          // TODO: Remove this hack when pseudo elements are implemented
          add(InlineBreak)
        } else {
          if (boxP.inlinyDomChildren != null) {
            boxP.inlinyDomChildren.foreach (dc => {
              if (dc.isInflow) {
                dc match {
                  case bwp: BoxWithProps =>
                    if (bwp.displayInner == "flow-root") {
                      bwp.formattingContext.foreach(fc => fc.layout(vwProps))
                      add(new FlowRootInlineRenderable(bwp))
                    } else {
                      dc.initProps(vwProps)
                      inlineLayoutRecurse(dc, ipc, vwProps)
                    }
                  case _ =>
                    dc.initProps(vwProps)
                    inlineLayoutRecurse(dc, ipc, vwProps)
                }
              } else {
                dc match {
                  case bp: BoxWithProps => handleOutOfFlow(bp)
                  case _ => ???
                }
              }
            })
          } else {
            boxP.boxyDomChildren.foreach(c => inlineLayoutRecurse(c, ipc, vwProps))
          }
        }
      case ab: AnonBox =>
        getSimpleInlineRenderables(ab, vwProps).foreach(add)
    }
  }

  private def getSimpleInlineRenderables(ab: AnonBox, vwProps: ViewPortProps) : Vector[InlineRenderable] = {
    val words = ab.getWords
    words.map(w => new InlineWordRenderable(w, ab.creator.b.visibility, ab.creator.colorProp, ab.creator.fontProp)).toVector
  }

  private def blockLayout(boxP: BoxWithProps, vwProps: ViewPortProps): Unit = {
    var yPos = 0
    // var maxWidth = 0
    boxP.boxyDomChildren.foreach{c =>
      if(config.layoutDebugLevel > 1) println(s"  layout of $c")
      if (c.isInflow) {
        c.b.offsetY = yPos

        layout(c, vwProps)

        yPos += c.b.marginBoxHeight
      } else {
        c.b.offsetY = Util.findCascadingOffsetY(boxP, c.containingBlock, yPos)
        c.b.offsetX = Util.findCascadingOffsetX(boxP, c.containingBlock, 0)
      }
    }
    val specHeight = boxP.size.height.specified match {
      case AutoLength => 0
      case AbsLength(pxs) => pxs.toInt
      case frl: FontRelLength => (frl.compute(boxP.fontProp)).toInt
      case ParentRelLength(prl) => (prl * boxP.b.contentHeight).toInt
      case _ => ???
    }
    boxP.b.contentHeight = math.max(specHeight, yPos)
  }

}

*/
