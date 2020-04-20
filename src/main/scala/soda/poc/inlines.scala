/*
package soda.poc

import java.awt.Graphics2D
import soda.layout.ViewPortProps

class InlineElemRenderable(val parent: Content) extends InlineRenderable {
  override def toString = s"box est: $estWidth x $estHeight"
  def paint(g: Graphics2D): Unit = {
    val gt = g.create(box.offsetX + box.renderOffsetX, box.offsetY + box.renderOffsetY, box.marginBoxWidth, box.marginBoxHeight).asInstanceOf[Graphics2D]
    box.paint(gt, null)
    gt.dispose()
  }
  val isBreak: Boolean = false

  val estWidth = AbsLength(box.marginBoxWidth)
  val estHeight = AbsLength(box.marginBoxHeight)
}
*/

/*
class FlowRootInlineRenderable(btn: BoxTreeNode) extends InlineRenderable {
  override def toString = s"flow root inline est: $estWidth x $estHeight"

  def estWidth: LengthSpec = AbsLength(btn.b.marginBoxWidth)

  def estHeight: LengthSpec = AbsLength(btn.b.marginBoxHeight)

  // val box: Box = new Box()

  val isBreak: Boolean = false

  def paint(g: Graphics2D): Unit = {
    val gt = g.create(box.offsetX + box.renderOffsetX, box.offsetY + box.renderOffsetY, btn.b.marginBoxWidth, btn.b.marginBoxHeight).asInstanceOf[Graphics2D]
    // gt.setColor(Color.CYAN)
    // gt.fillRect(0, 0, btn.b.marginBoxWidth, btn.b.marginBoxHeight)
    btn.paint(gt)
    gt.dispose()
  }

}
*/
