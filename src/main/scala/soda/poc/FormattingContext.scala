package soda.poc

import soda.analysis.DecoratedNode
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps

sealed trait FormattingContext {
  var establishingBox: BoxWithProps = null
  // def generateBoxes(): Unit
  def layout(vwProps: ViewPortProps): Unit

  def getFlowBoxType(displayOuter: String): InnerBoxType = BlockContainerBoxType
}


final class BlockFormattingContext(estBox: BoxWithProps) extends FormattingContext {

  override def getFlowBoxType(displayOuter: String): InnerBoxType = {
    if (displayOuter == "inline" || displayOuter == "run-in") {
      InlineBoxType
    } else {
      BlockContainerBoxType
    }
  }

  def layout(vwProps: ViewPortProps): Unit = { doLayout(estBox, vwProps) }

  private def layout(boxTN: BoxTreeNode, vwProps: ViewPortProps): Unit = {
    boxTN match {
      case boxP: BoxWithProps =>
        boxP.formattingContext match {
          case Some(fc) => fc.layout(vwProps)
          case _ => doLayout(boxP, vwProps)
        }
      case aib: AnonInlineBox => {
        // println("Anon inline")
        aib.inlineLayout(vwProps)
        // boxP.getInlineRenderables.foreach(boxP.addInlineRenderable)
      }
      case tr: TextRun => { println("This should probably never happend") }
      case x => println(x); ???
    }
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
          (width, compMarginLeft.get, rem)
        } else if (compMarginRight.isDefined) {
          val rem = avlWidth - (width + borderPaddingWidth + compMarginRight.get)
          (width, rem, compMarginRight.get)
        } else {
          val rem = avlWidth - (width + borderPaddingWidth)
          val remBy2 = rem/2
          (width, remBy2, rem - remBy2)
        }
      }

      val (usedWidth, marginLeft, marginRight) = compWidth match {
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
            (width, marginLeftElseZero, marginRightElseZero)
          }
        }
      }
      boxP.b.contentWidth = usedWidth
      boxP.b.marginThickness.left = marginLeft
      boxP.b.marginThickness.right = marginRight
      boxP.b.marginThickness.top = onlyPositive(compMarginTop.getOrElse(0))
      boxP.b.marginThickness.bottom = onlyPositive(compMarginBottom.getOrElse(0))
      // println(boxP.debugId + "  margin thickness: " + boxP.b.marginThickness)
      // println("  content width: " + usedWidth)
      // println("  border thickness: " + boxP.b.border)
    }

    // println("In : " + boxP.debugId)
    if (boxP.inlineMode) {
      // println("  inline mode")
      val heightModified = boxP.computeHeights()
      boxP.inlineLayout(!heightModified, vwProps)
    } else {
      // println("  block mode")
      blockLayout(boxP, vwProps)
    }
  }

  private def onlyPositive(x: Int) = if (x >= 0) x else 0

  private def blockLayout(boxP: BoxWithProps, vwProps: ViewPortProps): Unit = {
    var yPos = 0
    val parentOffsetY = boxP.b.contentOffsetY
    // var maxWidth = 0
    boxP.domChildren.foreach{c =>
      layout(c, vwProps)
      c match {
        case hb: HasBox => {
          hb.b.offsetY = yPos
          hb.b.offsetX = 0
          yPos += hb.b.marginBoxHeight
          /*
          if (hb.b.contentWidth > maxWidth) {
            maxWidth = hb.b.contentWidth
          }*/
        }
        case x => println("Probably shouldn't happen: " + x)
      }
    }
    val specHeight = boxP.size.height.specified match {
      case AutoLength => 0
      case AbsLength(pxs) => pxs.toInt
      case frl: FontRelLength => (frl.compute(boxP.fontProp)).toInt
      case ParentRelLength(prl) => (prl * boxP.b.contentHeight).toInt
    }
    boxP.b.contentHeight = math.max(specHeight, yPos)
  }
}


class TableFormattingContext() extends FormattingContext {
  // def generateBoxes(): Unit = ???
  
  def layout(vwProps: ViewPortProps) = ???
}

