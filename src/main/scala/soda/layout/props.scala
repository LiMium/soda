package soda.layout

import java.awt.Color
import java.awt.Font
import java.awt.FontMetrics
import cz.vutbr.web.css.NodeData
import soda.layout.ViewPortProps
import soda.layout.FontKey
import javax.imageio.ImageIO
import java.net.URL
import cz.vutbr.web.css.TermURI
import java.awt.image.BufferedImage
import java.net.MalformedURLException
import scala.util.Try
import cz.vutbr.web.css.TermList
import cz.vutbr.web.css.TermLengthOrPercent

class L1Property[ST, CT] {
  var specified: Option[ST] = None
  var computed: Option[CT] = None

}

object Property {
  def getSpec(nd: NodeData, key: String) = Option(nd.getAsString(key, true))
}

class Property[ST, CT, UT, AT] {
  var specified: Option[ST] = None
  var computed: Option[CT] = None
  var used: Option[UT] = None
  var actual: Option[AT] = None
}

abstract class PropertyWithInit[ST, CT, UT, AT] {
  var specified: ST
  var computed: Option[CT] = None
  var used: Option[UT] = None
  var actual: Option[AT] = None
}

sealed trait LengthSpec

case object AutoLength extends LengthSpec
case object NoneLength extends LengthSpec
case class AbsLength(pixels: Float) extends LengthSpec
case class FontRelLength(scale: Float, measure: String) extends LengthSpec {
  def compute(font: FontProp): Float = {
    if (measure == "ex") {
      font.exHeight * scale
    } else {
      font.emHeight * scale
    }
  }
}

case class PercentLength(scale: Float) extends LengthSpec

// TODO: With dotty we could split the border length into a different type hierarchy using unions
case object MediumLength extends LengthSpec
case object ThinLength extends LengthSpec
case object ThickLength extends LengthSpec

object LengthProp {
  val zeroAbsLength = AbsLength(0)

  def parseSpec(specStr: String, initial: LengthSpec = AutoLength): LengthSpec = {
    if (specStr != null) {
      if (specStr == "medium") {
        MediumLength
      } else if (specStr == "thick") {
        ThickLength
      } else if (specStr == "thin") {
        ThinLength
      } else if (specStr.endsWith("px")) {
        AbsLength(specStr.dropRight(2).toFloat)
      } else if (specStr.endsWith("%")) {
        PercentLength(specStr.dropRight(1).toFloat / 100)
      } else if (specStr.endsWith("in")) {
        // TODO: For now we assume a low DPI device and hence 1 inch == 96 pixels
        // This can be modified later to return a different class which can be resolved later into pixels
        // with view port properties
        AbsLength(specStr.dropRight(2).toFloat * 96)
      } else if (specStr.endsWith("pt")) {
        // TODO: Same as above
        AbsLength(specStr.dropRight(2).toFloat * 96 / 72.0f)
      } else if (specStr.endsWith("pc")) {
        // TODO: Same as above
        AbsLength(specStr.dropRight(2).toFloat * 96 / 6.0f)
      } else if (specStr.endsWith("cm")) {
        // TODO: Same as above
        AbsLength(specStr.dropRight(2).toFloat * 96 / 2.54f)
      } else if (specStr.endsWith("mm")) {
        // TODO: Same as above
        AbsLength(specStr.dropRight(2).toFloat * 96 / 25.4f)
      } else if (specStr.endsWith("ex")) {
        FontRelLength(specStr.dropRight(2).toFloat, "ex")
      } else if (specStr.endsWith("em")) {
        FontRelLength(specStr.dropRight(2).toFloat, "em")
      } else if (specStr == "auto") {
        AutoLength
      } else {
        soda.utils.Util.warnln("Unhandled length: " + specStr)
        // ???
        initial
      }
    } else {
      initial
    }
  }
}

class LengthProp(key: String, initial: LengthSpec) extends PropertyWithInit[LengthSpec, Float, Float, Int] {
  var specified = initial
  def init(nd: NodeData) = {
    val specStr = nd.getAsString(key, true)
    specified = LengthProp.parseSpec(specStr, initial)
  }
  override def toString = s"$key: $specified"
}

object ColorProp {
  def parseColor(colorStr: String, initial: Color): Color = {
    if (colorStr(0) == '#') {
      val r = Integer.decode("0x" + colorStr.substring(1, 3))
      val g = Integer.decode("0x" + colorStr.substring(3, 5))
      val b = Integer.decode("0x" + colorStr.substring(5, 7))
      new Color(r, g, b)
    } else if (colorStr == "transparent") {
      new Color(0x00000000, true)
    } else if (colorStr == "currentColor") {
      initial
    } else {
      soda.utils.Util.warnln("Unhandled: " + colorStr)
      initial
    }
  }
}

class ColorProp(key: String) {
  var specified: Color = null
  var computed: Color = null

  def init(nd: NodeData, fallback: Color) = {
    specified = Property.getSpec(nd, key).map(ColorProp.parseColor(_, null)).getOrElse(null)

    computed = if (specified == null) {
      fallback
    } else {
      specified
    }
  }

}

class L1StringProp(key: String, initial: String) extends L1Property[String, String] {
  def getSpec(nd: NodeData) = Option(nd.getAsString(key, true))

  def init(nd: NodeData) = {
    specified = getSpec(nd).orElse(Some(initial))
  }

  def get = {
    computed.getOrElse(specified.get)
  }
}

class StringProp(key: String, initial: String) extends Property[String, String, String, String] {
  def getSpec(nd: NodeData) = Option(nd.getAsString(key, true))

  def init(nd: NodeData) = {
    specified = getSpec(nd).orElse(Some(initial))
  }

  def get = {
    used.getOrElse(actual.getOrElse(computed.getOrElse(specified.get)))
  }
}

class SimpleStringProp(key: String, initial: String) {
  var specified: String = null
  def getSpec(nd: NodeData) = Option(nd.getAsString(key, true))
  def init(nd: NodeData) = {
    specified = getSpec(nd).getOrElse(initial)
  }
}

class SizeProps {
  val width = new LengthProp("width", AutoLength)
  val height = new LengthProp("height", AutoLength)
  val minWidth = new LengthProp("min-width", LengthProp.zeroAbsLength)
  val minHeight = new LengthProp("min-height", LengthProp.zeroAbsLength)
  val maxWidth = new LengthProp("max-width", NoneLength)
  val maxHeight = new LengthProp("max-height", NoneLength)

  val marginLeft = new LengthProp("margin-left", LengthProp.zeroAbsLength)
  val marginRight = new LengthProp("margin-right", LengthProp.zeroAbsLength)
  val marginTop = new LengthProp("margin-top", LengthProp.zeroAbsLength)
  val marginBottom = new LengthProp("margin-bottom", LengthProp.zeroAbsLength)

  val paddingLeft = new LengthProp("padding-left", LengthProp.zeroAbsLength)
  val paddingRight = new LengthProp("padding-right", LengthProp.zeroAbsLength)
  val paddingTop = new LengthProp("padding-top", LengthProp.zeroAbsLength)
  val paddingBottom = new LengthProp("padding-bottom", LengthProp.zeroAbsLength)

  def init(nd: NodeData) = {
    width.init(nd)
    height.init(nd)
    minWidth.init(nd)
    minHeight.init(nd)
    maxWidth.init(nd)
    maxHeight.init(nd)

    marginLeft.init(nd)
    marginRight.init(nd)
    marginTop.init(nd)
    marginBottom.init(nd)

    paddingLeft.init(nd)
    paddingRight.init(nd)
    paddingTop.init(nd)
    paddingBottom.init(nd)
  }

  override def toString = s"width: ${width} height: ${height}"
}

class BorderProp(side: String) {
  val color = new ColorProp(s"border-$side-color")
  val style = new StringProp(s"border-$side-style", "none")
  val width = new LengthProp(s"border-$side-width", MediumLength)

  def init(nd: NodeData, fallbackColor: Color) = {
    color.init(nd, fallbackColor)
    style.init(nd)
    width.init(nd)
  }
}

class FontSizeProp {
  var specified : String = null
}

class FontProp {
  var size = 0
  var font: Font = null
  var fontMetrics: FontMetrics = null
  var exHeight = 0
  var ascent = 0

  def init(nd: NodeData, pFontProp: Option[FontProp], vwProps: ViewPortProps) = {
    val computedSize = Property.getSpec(nd, "font-size") match {
      case None => pFontProp.map(_.size).getOrElse(vwProps.fontSizeMap("medium"))
      case Some(spec) => {
        if (vwProps.fontSizeMap.isDefinedAt(spec)) {
          vwProps.fontSizeMap(spec)
        } else if (spec.endsWith("px")) {
          spec.dropRight(2).toFloat
        } else if (spec.endsWith("%")) {
          val factor = spec.dropRight(1).toFloat
          factor * pFontProp.map(_.size).getOrElse(0)
        } else if (spec.endsWith("em")) {
          val factor = spec.dropRight(2).toFloat
          factor * pFontProp.map(_.emHeight).getOrElse(0)
        } else if (spec.endsWith("ex")) {
          val factor = spec.dropRight(2).toFloat
          factor * pFontProp.map(_.exHeight).getOrElse(0)
        } else if (spec.endsWith("in")) {
          // TODO: For now we assume a low DPI device and hence 1 inch == 96 pixels
          // This can be modified later to return a different class which can be resolved later into pixels
          // with view port properties
          spec.dropRight(2).toFloat * 96
        } else if (spec.endsWith("pt")) {
          // TODO: Same as above
          spec.dropRight(2).toFloat * 96 / 72.0f
        } else if (spec.endsWith("pc")) {
          // TODO: Same as above
          spec.dropRight(2).toFloat * 96 / 6.0f
        } else if (spec.endsWith("cm")) {
          // TODO: Same as above
          spec.dropRight(2).toFloat * 96 / 2.54f
        } else if (spec.endsWith("mm")) {
          // TODO: Same as above
          spec.dropRight(2).toFloat * 96 / 25.4f
        } else if (spec == "inherit") {
          pFontProp.map(_.size).getOrElse(0)
        } else if (spec == "larger") {
          // TODO: This actually needs to match the closest standard size
          pFontProp.map(_.size).getOrElse(0) * 1.2f
        } else if (spec == "smaller") {
          // TODO: This actually needs to match the closest standard size
          pFontProp.map(_.size).getOrElse(0) * 0.8f
        } else {
          println("font spec:" + spec)
          ???
        }
      }
    }
    size = computedSize.toInt
    val familyOpt = Property.getSpec(nd, "font-family")
    val family = unquoteSingle(familyOpt.getOrElse(vwProps.defaultFontFamily))
    font = vwProps.getFont(FontKey(
      family = family,
      size = size,
      style = 0
    ))
    fontMetrics = vwProps.getFontMetrics(font)

    // Following the same workaround as gngr
    exHeight = if (family.contains("Ahem")) {
      (0.8 * font.getSize2D).toInt
    } else {
      val glyphVector = font.createGlyphVector(fontMetrics.getFontRenderContext(), "xuwz")
      (glyphVector.getVisualBounds.getHeight).toInt
    }
    ascent = fontMetrics.getAscent()
  }

  def emHeight = size

  def estWidth(str: String) = fontMetrics.stringWidth(str)

  private def unquoteSingle(s: String): String = {
    if (s.length > 2) {
      if (s(0) == '\'' && s(s.length -1) == '\'') {
        s.substring(1, s.length-1)
      } else {
        s
      }
    } else {
      s
    }
  }
}

class BackgroundProps() {
  val color = new ColorProp("background-color")
  val repeat = new SimpleStringProp("background-repeat", "repeat")
  private var imgSpec: URL = null
  var posX: TermLengthOrPercent = null
  var posY: TermLengthOrPercent = null

  def init(nd: NodeData) = {
    color.init(nd, null)
    repeat.init(nd)
    val tu : TermURI = nd.getValue(classOf[TermURI], "background-image", false)
    if (tu != null) {
      imgSpec = try {
        // TODO: ASCII escape the value, but it's not simple. Refer: https://stackoverflow.com/q/724043
        new URL(tu.getBase(), tu.getValue())
      } catch {
        case _:MalformedURLException => println("Malformed URI: " + tu); null
      }

      val pos = nd.getValue(classOf[TermList], "background-position", false)
      if (pos != null) {
        posX = pos.get(0).asInstanceOf[TermLengthOrPercent]
        posY = pos.get(1).asInstanceOf[TermLengthOrPercent]
      } else {
        posX = null
        posY = null
      }
    }
  }

  def getBufImg: Option[BufferedImage] = {
    if (imgSpec == null) {
      None
    } else {
      Try(Option(ImageIO.read(imgSpec))).toOption.flatten
    }
  }
}
