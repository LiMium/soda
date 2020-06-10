package soda.layout

import java.awt.Graphics2D
import java.awt.Color

class Sides[T](initial: => T) {
  var top: T = initial
  var right: T = initial
  var bottom: T = initial
  var left: T = initial

  override def toString = s"$top;$right;$bottom;$left"

  def forEach(f: (String, T) => Unit) = {
    f("top", top)
    f("right", right)
    f("bottom", bottom)
    f("left", left)
  }

  def byName(name: String): T = {
    name match {
      case "top" => top
      case "right" => right
      case "bottom" => bottom
      case "left" => left
    }
  }
}

class SidesInt extends Sides(0) {
  def horiz = left + right
  def vert = top + bottom
}

case class Rect(x: Int, y: Int, width: Int, height: Int)

class Border {
  var thickness: Int = 0
  var color: Color = null
  var style: String = "none"

  def colorDump = if (color != null) s"RGB:${color.getRed},${color.getGreen},${color.getBlue}" else "transp"
  override def toString = if (style == "none") "[0]" else s"$thickness $style $colorDump"
}

class Box {
  var border: Sides[Border] = new Sides[Border](new Border) {

  }
  def borderVert = border.top.thickness + border.bottom.thickness
  def borderHoriz = border.left.thickness + border.right.thickness

  val marginThickness = new SidesInt
  var paddingThickness = new SidesInt

  var offsetX = 0
  var offsetY = 0

  var renderOffsetX = 0
  var renderOffsetY = 0

  def paintOffsetX = offsetX + renderOffsetX
  def paintOffsetY = offsetY + renderOffsetY

  var contentWidth = 0
  var contentHeight = 0

  def paddingBoxOffsetX = marginThickness.left + border.left.thickness
  def paddingBoxOffsetY = marginThickness.top + border.top.thickness

  def contentOffsetX = paddingBoxOffsetX + paddingThickness.left
  def contentOffsetY = paddingBoxOffsetY + paddingThickness.top

  def marginWidth = marginThickness.horiz
  def borderWidth = borderHoriz
  def paddingWidth = paddingThickness.horiz
  def paddingBoxWidth = paddingWidth + contentWidth
  def borderPaddingWidth = borderWidth + paddingWidth
  def borderBoxWidth = borderPaddingWidth + contentWidth
  def marginBoxWidth = marginWidth + borderBoxWidth
  def marginBoxSansContentWidth = marginWidth + borderWidth + paddingWidth

  def marginHeight = marginThickness.vert
  def borderHeight = borderVert
  def paddingHeight = paddingThickness.vert
  def paddingBoxHeight = paddingHeight + contentHeight
  def borderBoxHeight = borderHeight + paddingHeight + contentHeight
  def marginBoxHeight = marginHeight + borderBoxHeight
  def marginBoxSansContentHeight = marginHeight + borderHeight + paddingHeight

  def paint(g: Graphics2D, bgProps: BackgroundProps): Unit = {
    if (config.showBoxes) {
      g.setColor(Color.MAGENTA)
      g.drawRect(marginThickness.left, marginThickness.top, borderBoxWidth, borderBoxHeight)
    }

    if (bgProps != null) {
      if (bgProps.color.computed != null) {
        g.setColor(bgProps.color.computed)
        g.fillRect(marginThickness.left, marginThickness.top, borderBoxWidth, borderBoxHeight)
      }

      def calcOffset(pos: AbsOrPercent, width: Int) = {
        if (pos == null) {
          0
        } else if (pos.isPercent) {
          (pos.value * width/100).toInt
        } else {
          (pos.value).toInt
        }
      }

      bgProps.getBufImg foreach {bi =>
        val bgGraphics = g.create().asInstanceOf[Graphics2D]
        val bbWidth = borderBoxWidth
        val bbHeight = borderBoxHeight
        val pw = paddingBoxWidth
        val ph = paddingBoxHeight
        val iw = bi.getWidth()
        val ih = bi.getHeight()
        val rep = bgProps.repeat.specified

        val xCount = if (rep == "repeat" || rep == "repeat-x") 2 + math.ceil(bbWidth.toDouble / iw).toInt else 1
        val yCount = if (rep == "repeat" || rep == "repeat-y") 2 + math.ceil(bbHeight.toDouble / ih).toInt else 1

        val bLeft = border.left.thickness
        val bTop = border.top.thickness

        val imageX = calcOffset(bgProps.posX, pw - iw)
        val imageY = calcOffset(bgProps.posY, ph - ih)
        val imageX = calcOffset(bgProps.posXComputed, pw - iw)
        val imageY = calcOffset(bgProps.posYComputed, ph - ih)

        var startX = if (xCount == 1) bLeft+imageX else bLeft%iw - (iw - (imageX % iw))
        var startY = if (yCount == 1) bTop+imageY else bTop%ih - (ih - (imageY % ih))

        val endX = startX + (xCount * iw)
        val endY = startY + (yCount * ih)

        bgGraphics.setClip(marginThickness.left, marginThickness.top, bbWidth, bbHeight)
        for (y <- startY until endY by ih) {
          for (x <- startX until endX by iw) {
            bgGraphics.drawImage(bi, x, y, bi.getWidth(), bi.getHeight(), null)
          }
        }
        bgGraphics.dispose()
      }
    }

    val borderRect = Rect(marginThickness.left, marginThickness.top, borderBoxWidth, borderBoxHeight)
    paintBorder(g, borderRect)
  }

  private def paintBorder(g: Graphics2D, br: Rect) = {
    if (config.paintDebugLevel > 2) println(s"Painting border: $br")
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

