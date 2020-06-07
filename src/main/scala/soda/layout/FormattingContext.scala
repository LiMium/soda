package soda.layout

import soda.utils.Util
import java.awt.image.BufferedImage

case class PrefWidths(prefMinWidth: Int, prefWidth: Int)

trait FormattingContext {
  /**
    * Inner layout of content `c` involves two things:
    *   1. Outer layout of sub-contents of `c`
    *   2. Finalising the content width of `c`
    *
    * @param c content whose inner layout is to be done
    * @param constraints layout constraints
    * @returns Number of pixels to be advanced in vertical direction
    */
  def innerLayout(c: Content, marginCollapseTopAvl: Int, constraints: LayoutConstraints): Int

  /**
    * Compute the `preferred min width` and `preferred width` as per section 10.3.5 of CSS 2.2
    *
    * These are useful for shrink-fit calculation
    */
  def preferredWidths(c: Content): PrefWidths
}

// Currently requires an `img`, but can be abstracted later
final class SimpleReplacedFormattingContext(img: BufferedImage) extends FormattingContext {
  private val intrinsicWidth = { if (img != null) img.getWidth() else 0 }
  private val intrinsicHeight = { if (img != null) img.getHeight() else 0 }

  private def resolveDim(c: Content): (Int, Int) = {
    val widthOpt = c.resolveLength(c.props.width, c.containingWidth, autoValue = None, noneValue = None)
    val heightOpt = c.resolveLength(c.props.height, c.containingHeight, autoValue = None, noneValue = None)
    (widthOpt, heightOpt) match {
      case (Some(width), Some(height)) => (width, height)
      case (Some(width), None) => (width, ((width.toFloat/intrinsicWidth) * intrinsicHeight).toInt)
      case (None, Some(height)) => (((height.toFloat/intrinsicHeight) * intrinsicWidth).toInt, height)
      case (None, None) => (intrinsicWidth, intrinsicHeight)
    }
  }

  def innerLayout(c: Content, marginCollapseTopAvl: Int, constraints: LayoutConstraints): Int = {
    val (w, h) = resolveDim(c)
    c.box.contentWidth = w
    c.box.contentHeight = h
    c.miniContext = EmptyMiniContext
    h
  }

  def preferredWidths(c: Content): PrefWidths = {
    val (w,h) = resolveDim(c)
    PrefWidths(w, w)
  }
}

object FCUtil {
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
  def computeWidthMargins(widthOpt: Option[Int], avlWidth: Int, compMarginLeft: Option[Int], compMarginRight: Option[Int]) = {
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

  def positionAbsolutes(c: Content, lc: LayoutConstraints) = {
    c.absolutes.foreach {abs =>
      val cWidth = abs.containingWidth
      val cHeight = abs.containingHeight

      val absLC = new LayoutConstraints(FitToShrink(cWidth), FitToShrink(cHeight), lc.vwProps)
      abs.getFormattingContext().innerLayout(abs, 0, absLC)

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
  }

  private def marginTranslate(c: Content, m: LengthSpec): Option[Int] = {
    c.resolveLength(m, 0, None, Some(0))
  }

  private def constrainWidth(tentativeWidth: Int, compMinWidth: Int, compMaxWidth: Option[Int]) = {
    val widthMinChecked = math.max(compMinWidth, tentativeWidth)
    val widthMaxChecked = compMaxWidth.map(math.min(_, widthMinChecked)).getOrElse(widthMinChecked)
    (widthMaxChecked, widthMaxChecked != tentativeWidth)
  }

  def finaliseWidthMargins(c: Content, lc: LayoutConstraints): Int = {
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
      val pw = c.getFormattingContext.preferredWidths(c)
      val shrinkToFitWidth = math.min(math.max(pw.prefMinWidth, actualAvlWidth), pw.prefWidth)
      (shrinkToFitWidth, mLeft, mRight)
    }

    c.box.marginThickness.left = marginLeft
    c.box.marginThickness.right = marginRight

    val minWidth = c.resolveLength(c.props.compMinWidth, containingWidth).getOrElse(0)
    val maxWidth = c.resolveLength(c.props.compMaxWidth, containingWidth, autoValue = None, None)
    val (width, _) = constrainWidth(tentativeWidth, minWidth, maxWidth)

    c.box.contentWidth = width

    width
  }

}