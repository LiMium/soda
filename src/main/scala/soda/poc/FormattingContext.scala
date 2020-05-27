package soda.poc

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