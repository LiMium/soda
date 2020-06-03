package soda.layout

import java.awt.Graphics2D
import java.awt.Color

final private class TableCell(val c: Content) {

  def paint(g: Graphics2D): Unit = {
    c.paintAll(g)
  }

  def getDeclaredWidth(tableWidthOpt: Option[Int]): Option[Int] = {
    c.resolveLength(c.props.width, tableWidthOpt.map(_.toFloat), None, None).map(dw => dw + c.box.borderWidth + c.box.paddingWidth)
  }
}

final private class TableRow {
  private var cells = Vector[TableCell]()

  def addCell(c: Content): Unit = {
    c.hydrateSimpleProps()
    println("    Adding cell: " + c)

    val tc = new TableCell(c)
    cells :+= tc
  }

  def paint(g: Graphics2D): Unit = {
    cells foreach {_.paint(g)}
  }

  def findWidths(tableWidthOpt: Option[Int]): Vector[Int] = {
    val declaredWidths = cells.map(_.getDeclaredWidth(tableWidthOpt))
    val declaredTotalWidth = declaredWidths.map(_.getOrElse(0)).sum
    val avlWidth = math.max(tableWidthOpt.getOrElse(0), declaredTotalWidth)
    val remWidth = avlWidth - declaredTotalWidth
    val numUndeclared = declaredWidths.count(_.isEmpty)
    val remPerCell = if (numUndeclared > 0) { remWidth / numUndeclared } else { 0 }
    declaredWidths.map(_.getOrElse(remPerCell))
  }

  def layout(offsetY: Int, widths: Vector[Int], vwProps: ViewPortProps):Int = {
    var currX = 0
    var maxHeight = 0
    cells.zip(widths) foreach {case (cell, totalWidth) =>
      val c = cell.c
      val pbWidth = c.box.paddingWidth + c.box.borderWidth
      // TODO: remove max when negative widths are supported
      val w = math.max(0, totalWidth - pbWidth)
      c.box.contentWidth = w
      c.box.offsetX = currX
      c.box.offsetY = offsetY
      currX += w + pbWidth

      c.getFormattingContext().innerLayout(c, 0, new LayoutConstraints(FitAvailable(w), FitToShrink(0), vwProps))
      if (c.box.contentHeight > maxHeight) {
        maxHeight = c.box.contentHeight
      }
    }
    maxHeight
  }
}

final private class TableFixedLayoutMiniContext(tableContent: Content) extends MiniContext[Content] {
  private var rows = Vector[TableRow]()
  private var currY = 0

  override def add(c: Content): Unit = {
    val row = new TableRow()
    println("Adding row: " + c)
    c.getSubContent().foreach {cellContent =>
      if (cellContent.props.displayInner == "table-cell") {
        row.addCell(cellContent)
      }
    }
    rows :+= row
  }

  override def getHeight: Int = currY

  override def paint(g: Graphics2D): Unit = {
    rows.foreach {_.paint(g) }
  }

  override def getCurrPosXY(): (Int, Int) = ???

  override def isNotEmpty: Boolean = rows.nonEmpty

  def finishLayout(vwProps: ViewPortProps): Unit = {
    rows.headOption match {
      case Some(firstRow) =>
        val widths = firstRow.findWidths(tableContent.resolveLength(tableContent.props.width, tableContent.parent.box.contentWidth))
        println(widths)
        rows foreach {row =>
          val h = row.layout(currY, widths, vwProps)
          currY += h
        }

      case None =>
    }

  }
}

final class TableFormattingContext extends FormattingContext {

  override def innerLayout(c: Content, marginCollapseTopAvl: Int, constraints: LayoutConstraints): Int = {
    val mc = new TableFixedLayoutMiniContext(c)
    c.getSubContent() foreach {tc =>
      val tcInner = tc.props.displayInner
      if (tcInner == "table-row") {
        mc.add(tc)
      } else if (tcInner == "table-row-group") {
        tc.getSubContent() foreach {trgc =>
          if (trgc.props.displayInner == "table-row") {
            mc.add(trgc)
          }
        }
      }
    }
    mc.finishLayout(constraints.vwProps)
    c.miniContext = mc
    mc.getHeight
  }

  override def preferredWidths(c: Content): PrefWidths = {
    // TODO
    PrefWidths(0, 0)
  }

}
