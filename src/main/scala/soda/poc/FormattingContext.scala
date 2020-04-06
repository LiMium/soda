package soda.poc

import soda.analysis.DecoratedNode
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps

sealed trait FormattingContext {
  // def generateBoxes(): Unit
  def layout(vwProps: ViewPortProps): Unit

  def getFlowBoxType(displayOuter: String): InnerBoxType = BlockContainerBoxType
}

final class SimpleReplacedFormattingContext(estBox: BoxWithProps) extends FormattingContext {
  def layout(vwProps: ViewPortProps): Unit = {
    if (estBox.b.img != null) {
      estBox.b.contentWidth = estBox.b.img.getWidth()
      estBox.b.contentHeight = estBox.b.img.getHeight()
    }
  }

  override def getFlowBoxType(displayOuter: String): InnerBoxType = {BlockContainerBoxType}
}

final class BlockFormattingContext(estBox: BoxWithProps) extends FormattingContext {

  override def getFlowBoxType(displayOuter: String): InnerBoxType = {
    if (displayOuter == "inline" || displayOuter == "run-in") {
      InlineBoxType
    } else {
      BlockContainerBoxType
    }
  }

  def layout(vwProps: ViewPortProps): Unit = {
    if (config.layoutDebugLevel > 0) println(s"Starting BFC layout from $estBox")
    doLayout(estBox, vwProps)
  }

  private def layout(boxTN: BoxTreeNode, vwProps: ViewPortProps): Unit = {
    boxTN match {
      case boxP: BoxWithProps =>
        boxP.formattingContext match {
          case Some(fc) => fc.layout(vwProps)
          case _ => doLayout(boxP, vwProps)
        }
      case aib: AnonInlineBox => {
        inlineLayoutAIB(aib, vwProps)
      }
    }
  }

  private def inlineLayoutAIB(aib: AnonInlineBox, vwProps: ViewPortProps): Unit = {
    aib.inlinePseudoContext.maxWidth = aib.creator.b.contentWidth
    getSimpleInlineRenderables(aib.textRun, vwProps).foreach(aib.inlinePseudoContext.addInlineRenderable)
    aib.b.contentHeight = aib.inlinePseudoContext.getHeight
    aib.b.contentWidth = aib.inlinePseudoContext.getWidth
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
    getInlineRenderables(boxP, vwProps).foreach({
      case Left(ir) => ipc.addInlineRenderable(ir)
      case Right(outOfFlow) => outOfFlow match {
        case oboxP: BoxWithProps =>
          // println("Setting offset of inline out of flow", boxP)
          val cascadingOffsetY = Util.findCascadingOffsetY(boxP, oboxP.containingBlock, ipc.currPos)
          val cascadingOffsetX = Util.findCascadingOffsetX(boxP, oboxP.containingBlock, 0)
          oboxP.b.offsetY = cascadingOffsetY
          oboxP.b.offsetX = cascadingOffsetX
        case _ => ???
      }
    })
    if (heightUpdate) {
      b.contentHeight = ipc.getHeight
    }
    if (b.shrinkToFit) {
      b.contentWidth = math.min(ipc.getWidth, b.contentWidth)
    }
    // b.contentWidth = inlinePseudoContext.getWidth
  }

  def getInlineRenderables(boxP: BoxWithProps, vwProps: ViewPortProps): Vector[Either[InlineRenderable, BoxTreeNode]] = {
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
        Vector(Left(new InlineElemRenderable(boxP.b)))
      } else {
        Vector.empty
      }
    } else if (boxP.tag == "br") {
      // TODO: Remove this hack when pseudo elements are implemented
      Vector(Left(InlineBreak))
    } else {
      // inflowChildren.flatMap {_.getInlineRenderables(vwProps)}
      if (boxP.inlinyDomChildren != null) {
        boxP.inlinyDomChildren.flatMap (dc => {
          if (dc.isInflow) {
            dc match {
              case bwp: BoxWithProps =>
                if (bwp.displayInner == "flow-root") {
                  bwp.formattingContext.foreach(fc => fc.layout(vwProps))
                  Vector(Left(new FlowRootInlineRenderable(bwp)))
                } else {
                  dc.initProps(vwProps)
                  getAllInlineRenderables(dc, vwProps)
                }
              case _ =>
                dc.initProps(vwProps)
                getAllInlineRenderables(dc, vwProps)
            }
          } else {
            dc match {
              case bp: BoxWithProps => Vector(Right(bp))
              case _ => ???
            }
          }
        })
      } else {
        boxP.boxyDomChildren.flatMap(getAllInlineRenderables(_, vwProps))
      }
    }
  }

  def getAllInlineRenderables(btn: BoxTreeNode, vwProps: ViewPortProps) = {
    btn match {
      case tr: TextRun => getInlineRenderables(tr, vwProps)
      case bwp: BoxWithProps => getInlineRenderables(bwp, vwProps)
      case aib: AnonInlineBox => getInlineRenderables(aib, vwProps)
    }
  }

  def getAllInlineRenderables(ir: InlineSource, vwProps: ViewPortProps) = {
    ir match {
      case tr: TextRun => getInlineRenderables(tr, vwProps)
      case bwp: BoxWithProps => getInlineRenderables(bwp, vwProps)
      case aib: AnonInlineBox => getInlineRenderables(aib, vwProps)
    }
  }

  private def getSimpleInlineRenderables(tr: TextRun, vwProps: ViewPortProps) : Vector[InlineRenderable] = {
    val words = tr.getWords
    words.map(w => new InlineWordRenderable(w, tr.boxP.b.visibility, tr.boxP.colorProp, tr.boxP.fontProp)).toVector
  }

  private def getInlineRenderables(tr: TextRun, vwProps: ViewPortProps) : Vector[Either[InlineRenderable, BoxTreeNode]] = {
    getSimpleInlineRenderables(tr, vwProps).map(Left(_))
  }

  private def getInlineRenderables(aib: AnonInlineBox, vwProps: ViewPortProps): Vector[Either[InlineRenderable, BoxTreeNode]] = {
    getInlineRenderables(aib.textRun, vwProps)
  }


  private def onlyPositive(x: Int) = if (x >= 0) x else 0

  private def blockLayout(boxP: BoxWithProps, vwProps: ViewPortProps): Unit = {
    var yPos = 0
    // var maxWidth = 0
    boxP.boxyDomChildren.foreach{c =>
      if(config.layoutDebugLevel > 1) println(s"  layout of $c")
      if (c.isInflow) {
        c match {
          case hb: HasBox => {
            hb.b.offsetY = yPos
          }
          case x => println("Probably shouldn't happen: " + x)
        }

        layout(c, vwProps)

        c match {
          case hb: HasBox => {
            yPos += hb.b.marginBoxHeight
          }
          case x => println("Probably shouldn't happen: " + x)
        }
      } else {
        c match {
          case hb: HasBox => {
            hb.b.offsetY = Util.findCascadingOffsetY(boxP, c.containingBlock, yPos)
            hb.b.offsetX = Util.findCascadingOffsetX(boxP, c.containingBlock, 0)
          }
          case _ => ???
        }
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

  private def absLayout(btn: BoxTreeNode, vwProps: ViewPortProps): Unit = {
    btn match {
      case hac: HasAbsChildren =>
        hac.getAbsChildren.foreach {c =>
          c.computeL2Props(vwProps)
          layout(c, vwProps)
        }
      case _ =>
    }
  }
}


class TableFormattingContext() extends FormattingContext {
  // def generateBoxes(): Unit = ???

  def layout(vwProps: ViewPortProps) = ???
}

