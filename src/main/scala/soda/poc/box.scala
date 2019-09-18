package soda.poc

import java.awt.Graphics2D
import java.awt.Color
import java.awt.image.BufferedImage

class Box {
  val border = new Sides[Border](new Border) {

    def vert = top.thickness + bottom.thickness
    def horiz = left.thickness + right.thickness
  }

  val marginThickness = new SidesInt
  val paddingThickness = new SidesInt

  val startPos = new Position
  var img: BufferedImage = null

  var offsetX = 0
  var offsetY = 0

  var renderOffsetX = 0
  var renderOffsetY = 0

  var contentWidth = 0
  var contentHeight = 0
  var visibility: Boolean = true
  var overflowX: String = "visible"
  var overflowY: String = "visible"

  def contentOffsetX = marginThickness.left + border.left.thickness + paddingThickness.left
  def contentOffsetY = marginThickness.top + border.top.thickness + paddingThickness.top

  def marginWidth = marginThickness.horiz
  def borderWidth = border.horiz
  def paddingWidth = paddingThickness.horiz
  def borderPaddingWidth = borderWidth + paddingWidth
  def borderBoxWidth = borderPaddingWidth + contentWidth
  def marginBoxWidth = marginWidth + borderBoxWidth
  def marginBoxSansContentWidth = marginWidth + borderWidth + paddingWidth

  def marginHeight = marginThickness.vert
  def borderHeight = border.vert
  def paddingHeight = paddingThickness.vert
  def borderBoxHeight = borderHeight + paddingHeight + contentHeight
  def marginBoxHeight = marginHeight + borderBoxHeight
  def marginBoxSansContentHeight = marginHeight + borderHeight + paddingHeight

  def paint(g: Graphics2D, bgColor: Color): Unit = {
    if (visibility) {
      // g.setColor(backgroundColor.actual.get)
      // val borderWidth = border.left.actual.get + border.right.actual.get
      // val paddingWidth = padding.left.actual.get + padding.right.actual.get
      //
      if (bgColor != null) {
        g.setColor(bgColor)
        g.fillRect(marginThickness.left, marginThickness.top, borderBoxWidth, borderBoxHeight)
      }

      // Paint left border
      val borderRect = Rect(marginThickness.left, marginThickness.top, borderBoxWidth, borderBoxHeight)
      paintBorder(g, borderRect)

      if (img != null) {
        val w = img.getWidth
        val h = img.getHeight
        // g.drawImage(img, 0, 0, w, h, null)
        g.drawImage(img, contentOffsetX, contentOffsetY, contentWidth, contentHeight, null)
      }
    }
  }

  private def paintBorder(g: Graphics2D, br: Rect) = {
    // println("Painting border: " + br + " with thickness: " + borderThickness + " color: " + borderColor)
    paintBorderHoriz(g, border.top.color, border.top.style, br.x, br.y, br.width, border.top.thickness)
    paintBorderHoriz(g, border.bottom.color, border.bottom.style, br.x, br.y+br.height - border.bottom.thickness, br.width, border.bottom.thickness)

    paintBorderVert(g, border.left.color, border.left.style, br.x, br.y, br.height, border.left.thickness)
    paintBorderVert(g, border.right.color, border.right.style, br.x+br.width - border.right.thickness, br.y, br.height, border.right.thickness)
  }

  private def paintBorderHoriz(g: Graphics2D, color: Color, style: String, x: Int, y: Int, width: Int, thickness: Int) = {
    if (width > 0 && thickness > 0) {
      style match {
        case "none" =>
        case "solid" => {
          g.setColor(color)
          for (i <- 0 until thickness) {
            g.drawLine(x, y+i, x+width-1, y+i)
          }
        }
        case _ =>
      }
    }
  }

  private def paintBorderVert(g: Graphics2D, color: Color, style: String, x: Int, y: Int, height: Int, thickness: Int) = {
    if (height > 0 && thickness > 0) {
      style match {
        case "none" =>
        case "solid" => {
          g.setColor(color)
          for (i <- 0 until thickness) {
            g.drawLine(x+i, y, x+i, y + height-1)
          }
        }
        case _ =>
      }
    }
  }

}

