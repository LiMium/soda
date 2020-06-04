package soda.layout

import cz.vutbr.web.css.NodeData
import java.awt.Color
import java.awt.Font
import java.awt.Graphics2D

case class ViewPortProps(width: Int, height: Int, dpi: Int, g2: Graphics2D) {
  def vmin = math.min(width, height)
  def vmax = math.max(width, height)

  val fontSizeMap = Map(
    "xx-small" -> 8,
    "x-small" -> 10,
    "small" -> 12,
    "medium" -> 16,
    "large" -> 18,
    "x-large" -> 22,
    "xx-large" -> 36,
  )

  val defaultFontFamily = "Helvetica"

  def getFont(fKey: FontKey): Font = {
    new Font(fKey.family, fKey.style, fKey.size)
  }

  def getFontMetrics(font: Font) = {
    g2.getFontMetrics(font)
  }

  def getLineMetrics(font: Font, str: String) = {
    font.getLineMetrics(str, g2.getFontRenderContext())
  }

  val borderSizeThick = 18
  val borderSizeMedium = 12
  val borderSizeThin = 6
}

case class FontProps(capHeight: Float, em: Float, exHeight: Float, lineHeight: Float)

sealed trait Size

/* A size that doesn't depend on other properties.
 * Dependencies on Viewport properties are accepted.
 * */
case class ConstantSize(pixels: Float) extends Size

case class FontRelativeSize(scale: Float, ref: String) extends Size {
  def computePixels(fontProps: FontProps, rootFontProps: FontProps) = {
    ref match {
      case "em" => fontProps.em*scale
      case "ex" => fontProps.exHeight*scale
    }
  }
}

case class ContainerRelativeSize(ratio: Float) extends Size {
  def computePixels(containerSize: Float) = {
    ratio * containerSize
  }
}

case class InheritSize() extends Size {
  def computePixels(containerSize: Float) = {
    containerSize
  }
}

sealed trait FontSize

/* A size that doesn't depend on other properties.
 * Dependencies on Viewport properties are accepted.
 * */
case class ConstantFontSize(pixels: Float) extends FontSize

case class FontRelativeFontSize(scale: Float, ref: String) extends FontSize {
  def computePixels(fontProps: FontProps, rootFontProps: FontProps) = {
    ref match {
      case "em" => fontProps.em*scale
      case "ex" => fontProps.exHeight*scale
    }
  }
}

case class ContainerRelativeFontSize(ratio: Float) extends FontSize {
  def computePixels(containerSize: Float) = {
    ratio * containerSize
  }
}

case class InheritFontSize() extends FontSize {
  def computePixels(containerSize: Float) = {
    containerSize
  }
}

final class Properties(nodeData: NodeData, parentProperties: Option[Properties]) {
  override def toString = "" + nodeData
  
  def getDisplay: String = {
    getAsString("display")
  }
  def getBackgroundColor: String = {
    getAsString("background-color")
  }
  def getColor: String = {
    getAsString("color")
  }

  // TODO
  private val fontSize = 20
  private val fontXHeight = 16

  def parseFontSize(sizeStr: String): Option[FontSize] = {
    def trimRight(n: Int) = {
      sizeStr.substring(0, sizeStr.length - n)
    }
    try {
      if (sizeStr.endsWith("px")) {
        Some(ConstantFontSize(trimRight(2).toFloat))
      } else if (sizeStr.endsWith("em")) {
        Some(FontRelativeFontSize(trimRight(2).toFloat, "em"))
      } else if (sizeStr.endsWith("ex")) {
        Some(FontRelativeFontSize(trimRight(2).toFloat, "ex"))
      } else if (sizeStr.endsWith("cm")) {
        Some(ConstantFontSize(trimRight(2).toFloat * 96f/2.54f))
      } else if (sizeStr.endsWith("mm")) {
        Some(ConstantFontSize(trimRight(2).toFloat * 96f/25.4f))
      } else if (sizeStr.endsWith("pt")) {
        Some(ConstantFontSize(trimRight(2).toFloat * 96f/72f))
      } else if (sizeStr.endsWith("pc")) {
        Some(ConstantFontSize(trimRight(2).toFloat * 16f))
      } else if (sizeStr.endsWith("in")) {
        Some(ConstantFontSize(trimRight(2).toFloat * 96f))
      } else if (sizeStr.endsWith("%")) {
        Some(ContainerRelativeFontSize(trimRight(2).toFloat/100f))
      } else if (sizeStr.equals("inherit")) {
        Some(InheritFontSize())
      } else if (sizeStr.equals("auto")) {
        None
      } else {
        println("Unhandled size: " + sizeStr)
        Some(ConstantFontSize(0))
      }
    } catch {
      case n: NumberFormatException => {
        println("Error while parsing: " + sizeStr)
        None
      }
    }
  }
  def parseSize(sizeStr: String): Option[Size] = {
    def trimRight(n: Int) = {
      sizeStr.substring(0, sizeStr.length - n)
    }
    try {
      if (sizeStr.endsWith("px")) {
        Some(ConstantSize(trimRight(2).toFloat))
      } else if (sizeStr.endsWith("em")) {
        Some(FontRelativeSize(trimRight(2).toFloat, "em"))
      } else if (sizeStr.endsWith("ex")) {
        Some(FontRelativeSize(trimRight(2).toFloat, "ex"))
      } else if (sizeStr.endsWith("cm")) {
        Some(ConstantSize(trimRight(2).toFloat * 96f/2.54f))
      } else if (sizeStr.endsWith("mm")) {
        Some(ConstantSize(trimRight(2).toFloat * 96f/25.4f))
      } else if (sizeStr.endsWith("pt")) {
        Some(ConstantSize(trimRight(2).toFloat * 96f/72f))
      } else if (sizeStr.endsWith("pc")) {
        Some(ConstantSize(trimRight(2).toFloat * 16f))
      } else if (sizeStr.endsWith("in")) {
        Some(ConstantSize(trimRight(2).toFloat * 96f))
      } else if (sizeStr.endsWith("%")) {
        Some(ContainerRelativeSize(trimRight(2).toFloat/100f))
      } else if (sizeStr.equals("inherit")) {
        Some(InheritSize())
      } else if (sizeStr.equals("auto")) {
        None
      } else {
        println("Unhandled size: " + sizeStr)
        Some(ConstantSize(0))
      }
    } catch {
      case n: NumberFormatException => {
        println("Error while parsing: " + sizeStr)
        None
      }
    }
  }

  def parseSize1(sizeStr: String): Float = {
    def trimRight(n: Int) = {
      sizeStr.substring(0, sizeStr.length - n)
    }
    try {
      if (sizeStr.endsWith("px")) {
        trimRight(2).toFloat
      } else if (sizeStr.endsWith("em")) {
        trimRight(2).toFloat * fontSize
      } else if (sizeStr.endsWith("ex")) {
        trimRight(2).toFloat * fontXHeight
      } else if (sizeStr.endsWith("in")) {
        trimRight(2).toFloat * 96
      } else {
        println("Unhandled: " + sizeStr)
        0
      }
    } catch {
      case n: NumberFormatException => {
        println("Error while parsing: " + sizeStr)
        0
      }
    }
  }

  def getBorderWidth(direction: String): Option[Size] = {
    getAsStringOpt("border-"+direction+"-width").flatMap(parseSize)
  }

  def parseColor(colorStr: String): Color = {
    if (colorStr(0) == '#') {
      val r = Integer.decode("0x" + colorStr.substring(1, 3))
      val g = Integer.decode("0x" + colorStr.substring(3, 5))
      val b = Integer.decode("0x" + colorStr.substring(5, 7))
      new Color(r, g, b)
    } else {
      Color.BLACK
    }
  }

  def getBorderColor(direction: String): Color = {
    val str = getAsString("border-"+direction+"-color")
    if (str != null) {
      parseColor(str)
    } else {
      Color.BLACK
    }
  }

  private def getSized(s: String) = getAsStringOpt(s).flatMap(parseSize)

  def getWidth = getSized("width")
  def getMaxWidth = getSized("max-width")
  def getMinWidth = getSized("min-width")

  def getHeight = getSized("height")
  def getMaxHeight = getSized("max-height")
  def getMinHeight = getSized("min-height")

  def getFontSize = getAsStringOpt("font-size").flatMap(parseFontSize)

  def getAsStringOpt(name: String) = {
    if (nodeData == null) {
      None
    } else {
      Option(nodeData.getAsString(name, true))
    }
  }

  def getAsString(name: String) = {
    if (nodeData == null) {
      null
    } else {
      nodeData.getAsString(name, true)
    }
  }
}
