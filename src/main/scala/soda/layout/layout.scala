package soda.layout

import scala.language.existentials
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Font
import java.awt.Graphics2D
import java.awt.image.BufferedImage

import cz.vutbr.web.css.NodeData
import cz.vutbr.web.css.RuleSet
import cz.vutbr.web.domassign.Analyzer.Holder
import soda.analysis.Analysis
import soda.dom.RenderableDocument
import soda.dom.RenderableElement
import soda.dom.RenderableNode
import soda.dom.RenderableText

final class Point {
  var x = 0f
  var y = 0f
}

final class Dimension {
  var width = 0f
  var height = 0f
  override def toString = s"$width x $height"
}

final class Thickness {
  var top = 0
  var bottom = 0
  var left = 0
  var right = 0

  def horizThickness = left + right
  def verticalThickness = top + bottom
}

final class LayoutData() {
  val position = new Point()
  val size = new Dimension()
  val bgColor: Color = null
}

case class FontKey(family: String, style: Int, size: Int)
