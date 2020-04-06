package soda.poc

import java.awt.Graphics2D

trait InlineRenderable {
  def paint(g: Graphics2D): Unit
  def estWidth: Int
  def estHeight: Int
  val box: Box
  val isBreak: Boolean
  val isSpace: Boolean = false
}

object InlineBreak extends InlineRenderable {
  def paint(g: Graphics2D): Unit = {}
  val estWidth: Int = 0
  val estHeight: Int = 0
  val box: Box = null
  val isBreak: Boolean = true
}

class InlineElemRenderable(val box: Box) extends InlineRenderable {
  override def toString = s"box est: $estWidth x $estHeight"
  def paint(g: Graphics2D): Unit = {
    val gt = g.create(box.offsetX + box.renderOffsetX, box.offsetY + box.renderOffsetY, box.marginBoxWidth, box.marginBoxHeight).asInstanceOf[Graphics2D]
    box.paint(gt, null)
    gt.dispose()
  }
  val isBreak: Boolean = false

  val estWidth = box.marginBoxWidth
  val estHeight = box.marginBoxHeight
}

class FlowRootInlineRenderable(btn: BoxTreeNode) extends InlineRenderable {
  override def toString = s"flow root inline est: $estWidth x $estHeight"

  def estWidth: Int = btn.b.marginBoxWidth

  def estHeight: Int = btn.b.marginBoxHeight

  val box: Box = new Box()

  val isBreak: Boolean = false

  def paint(g: Graphics2D): Unit = {
    val gt = g.create(box.offsetX + box.renderOffsetX, box.offsetY + box.renderOffsetY, btn.b.marginBoxWidth, btn.b.marginBoxHeight).asInstanceOf[Graphics2D]
    // gt.setColor(Color.CYAN)
    // gt.fillRect(0, 0, btn.b.marginBoxWidth, btn.b.marginBoxHeight)
    btn.paint(gt)
    gt.dispose()
  }

}

class InlineWordRenderable(word: String, visibility: Boolean, colorProp: ColorProp, fontProp: FontProp) extends InlineRenderable {
  override def toString = s"word '$word' est: $estWidth x $estHeight"
  val box = new Box()
  def paint(g: Graphics2D): Unit = {
    if (visibility) {
      g.setColor(colorProp.computed)
      g.setFont(fontProp.font)
      g.drawString(word, box.offsetX, box.offsetY + fontProp.ascent)
    }
  }

  val estWidth = fontProp.estWidth(word)
  val estHeight = fontProp.size
  val isBreak: Boolean = false
  override val isSpace: Boolean = word == " "
}

class Line(val yPos: Int) {
  var renderables = Vector[InlineRenderable]()
  private val space = 4
  var height = 0
  var width = 0

  def willFit(ir: InlineRenderable, maxWidth: Int) = {
    val requiredSpace = ir.estWidth
    (requiredSpace + width) <= maxWidth
  }

  private def shouldIgnore(ir: InlineRenderable) = {
    if (ir.isSpace) {
      renderables.lastOption.map(_.isSpace).getOrElse(true)
    } else {
      false
    }
  }

  def add(ir: InlineRenderable) = {
    if (!shouldIgnore(ir)) {
      ir.box.offsetX = width
      ir.box.offsetY = yPos

      val iwidth = ir.estWidth
      height = math.max(height, ir.estHeight)
      width += iwidth
      renderables = renderables.appended(ir)
    }
  }

  def getCurrPosX = width
}

class InlinePseudoContext {
  var maxWidth = 0
  var lines = Vector[Line]()
  var currLine:Line = null
  private var currPosY = 0

  def getCurrPosXY(): (Int, Int) = {
    if (currLine == null) {
      (0, 0)
    } else {
      ( currLine.getCurrPosX, currPosY)
    }
  }

  private def startNewLine() = {
    if (currLine != null) {
      currPosY += currLine.height
    }
    currLine = new Line(currPosY)
    lines = lines.appended(currLine)
  }

  def addInlineRenderable(ir: InlineRenderable) = {
    if (config.layoutDebugLevel > 2) println(s"Adding ir $ir")

    if (ir.isBreak) {
      startNewLine()
    } else {
      if (currLine == null) {
        startNewLine()
      } else if (currLine.width != 0 && !currLine.willFit(ir, maxWidth)) {
        startNewLine()
      }
      currLine.add(ir)
    }
  }

  def paint(g: Graphics2D): Unit = {
    lines.foreach {l =>
      l.renderables.foreach{_.paint(g)}
    }
  }

  def getHeight = lines.map(_.height).sum
  def getWidth = lines.map(_.width).maxOption.getOrElse(0)
}

