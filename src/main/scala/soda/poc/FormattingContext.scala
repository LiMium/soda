package soda.poc

import soda.utils.Util

case class PrefWidths(prefMinWidth: Int, prefWidth: Int)

trait FormattingContext {
  /**
    * Inner layout of content `c` involves two things:
    *   1. Outer layout of sub-contents of `c`
    *   2. Finalising the content width of `c`
    *
    * @param c content whose inner layout is to be done
    * @param constraints layout constraints
    */
  def innerLayout(c: Content, constraints: LayoutConstraints): Unit

  /**
    * Compute the `preferred min width` and `preferred width` as per section 10.3.5 of CSS 2.2
    *
    * These are useful for shrink-fit calculation
    */
  def preferredWidths(c: Content): PrefWidths
}

final class SimpleReplacedFormattingContext extends FormattingContext {
  private def resolveWidth(c: Content) = {
    c.props.width match {
      case AbsLength(pixels) => pixels.toInt
      case PercentLength(scale) => (c.containingWidth*scale).toInt
      case x => Util.warnln("unhandled width for replaced elem:" + x);???
    }
  }

  def innerLayout(c: Content, constraints: LayoutConstraints): Unit = {
    val replacedWidth = resolveWidth(c)
    val replacedHeight = c.props.height match {
      case AbsLength(pixels) => pixels.toInt
      case PercentLength(scale) => (c.containingHeight*scale).toInt
      case x => Util.warnln("unhandled height for replaced elem:" + x);???
    }
    c.box.contentWidth = replacedWidth
    c.box.contentHeight = replacedHeight
    c.miniContext = EmptyMiniContext
  }

  def preferredWidths(c: Content): PrefWidths = {
    val w = resolveWidth(c)
    PrefWidths(w, w)
  }
}