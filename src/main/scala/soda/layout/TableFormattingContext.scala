package soda.layout

import java.awt.Graphics2D
import java.awt.Color
import soda.utils.Util

final private class TableCell(val c: Content) {

  def paint(g: Graphics2D): Unit = {
    c.paintAll(g)
  }

  /* def getDeclaredTotalWidth(tableWidthOpt: Option[Int]): Option[Int] = {
    c.resolveLength(c.props.width, tableWidthOpt.map(_.toFloat), None, None).map(dw => dw + c.box.borderWidth + c.box.paddingWidth)
  }*/

  def getDeclaredTotalWidth(tableWidthOpt: Option[Int]): (Option[Int], Int) = {
    val cwOpt = c.resolveLength(c.props.width, tableWidthOpt.map(_.toFloat), None, None).map(dw => dw + c.box.borderWidth + c.box.paddingWidth)

    cwOpt -> c.box.marginBoxSansContentWidth
  }

  def findPrefWidths() = {
    val fc = c.getFormattingContext()
    if (fc == null) {
      println("fc null for: " + c)
      val w = c.resolveLength(c.props.width, 0).getOrElse(0)
      PrefWidths(w, w)
    } else {
      c.getFormattingContext().preferredWidths(c, true)
    }
  }

  def getMarginBoxHeight = c.box.marginBoxHeight
}

final private class TableRow(startX: Int, c: Content) {
  private var cells = Vector[TableCell]()

  def addCell(c: Content): Unit = {
    c.hydrateSimpleProps()
    println("      Adding cell: " + c)

    val tc = new TableCell(c)
    cells :+= tc
  }

  def paint(g: Graphics2D): Unit = {
    c.paintAll(g)
    cells foreach {_.paint(g)}
  }

  /*def declWidths(tableWidthOpt: Option[Int]): Vector[Option[Int]] = {
    cells.map(_.getDeclaredTotalWidth(tableWidthOpt))
  }*/
  def declWidths(tableWidthOpt: Option[Int]): Vector[(Option[Int], Int)] = {
    cells.map(_.getDeclaredTotalWidth(tableWidthOpt))
  }

  def findWidths(tableWidthOpt: Option[Int]): Vector[Int] = {
    val declaredWidths = declWidths(tableWidthOpt)
    val declaredTotalWidth = declaredWidths.map(_._1.getOrElse(0)).sum
    // val declaredTotalWidth = declWidths(tableWidthOpt).sum
    val avlWidth = math.max(tableWidthOpt.getOrElse(0), declaredTotalWidth)
    val remWidth = avlWidth - declaredTotalWidth
    val numUndeclared = declaredWidths.count(_._1.isEmpty)
    val remPerCell = if (numUndeclared > 0) { remWidth / numUndeclared } else { 0 }
    declaredWidths.map(wt => wt._1.getOrElse(remPerCell) + wt._2)
  }

  def findPrefWidths(): Vector[Int] = {
    cells.map(_.findPrefWidths()).map(_.prefWidth)
  }


  def layout(offsetY: Int, widths: Vector[Int], vwProps: ViewPortProps):Int = {
    var currX = startX
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

      val fc = c.getFormattingContext()
      if (fc != null) {
        fc.innerLayout(c, 0, new LayoutConstraints(FitAvailable(w), FitToShrink(0), vwProps))
      }
      if (c.box.contentHeight > maxHeight) {
        maxHeight = c.box.contentHeight
      }
    }

    // layout the row
    c.hydrateSimpleProps()
    c.box.contentWidth = currX
    c.box.contentHeight = cells.map(_.getMarginBoxHeight).maxOption.getOrElse(0)

    maxHeight
  }

  def setYOffset(yOffset: Int) = {
    c.box.offsetY = yOffset
  }
  def getMarginBoxWidth = c.box.marginBoxWidth
  def getMarginBoxHeight = c.box.marginBoxHeight
}

/**
  * Represents all types of row groups including table-header-group, table-row-group and table-footer-group
  *
  * @param startX
  */
final private class TableRowGroup(startX: Int, trgc: Content) {
  private var rows = Vector[TableRow]()
  private val groupStartX = startX + trgc.box.contentOffsetX

  def firstRowOption = rows.headOption

  def addRow(c: Content): Unit = {
    c.hydrateSimpleProps()
    println("    Adding row: " + c)

    val tr = new TableRow(groupStartX, c)
    c.getSubContent().foreach(cell =>
      tr.addCell(cell)
    )
    rows :+= tr
  }

  def getRows = rows

  def paint(g: Graphics2D): Unit = {
    trgc.box.paint(g, trgc.renderProps.bgProps)
    rows foreach {_.paint(g)}
  }

  def declWidths(tableWidthOpt: Option[Int]): Vector[(Option[Int], Int)] = {
    rows.flatMap(_.declWidths(tableWidthOpt))
  }

  def findPrefWidths() = {
    rows.map(_.findPrefWidths())
  }

  def finishLayout() = {
    trgc.box.contentWidth = rows.map(_.getMarginBoxWidth).maxOption.getOrElse(0)
    var currY = 0
    for (row <- rows) {
      row.setYOffset(currY)
      currY += row.getMarginBoxHeight
    }
    trgc.box.contentHeight = currY
  }

  def getMarginBoxWidth = trgc.box.marginBoxWidth
}

final private class TableFixedLayoutMiniContext(tableContent: Content) extends MiniContext[Content] {
  private var rowGroups = Vector[TableRowGroup]()
  private var currY = tableContent.box.contentOffsetY
  // TODO: Remove startX here and as parameter to TableRowGroup, because it is always zero
  private val startX = 0

  override def add(c: Content): Unit = {
    c.hydrateSimpleProps()
    val rowGroup = new TableRowGroup(startX, c)
    println("Adding row group: " + c)
    c.getSubContent().foreach {rowContent =>
      if (rowContent.props.displayInner == "table-row") {
        rowGroup.addRow(rowContent)
      }
    }
    rowGroups :+= rowGroup
  }

  override def getHeight: Int = currY
  def getWidth: Int = rowGroups.map(_.getMarginBoxWidth).maxOption.getOrElse(0)

  override def paint(g: Graphics2D): Unit = {
    // tableContent.box.paint(g, tableContent.renderProps.bgProps)
    rowGroups.foreach {_.paint(g) }
  }

  override def getCurrPosXY(): (Int, Int) = ???

  override def isNotEmpty: Boolean = rowGroups.nonEmpty

  def finishLayout(vwProps: ViewPortProps): Unit = {
    val tableWidthOpt = tableContent.resolveLength(tableContent.props.width, Some(tableContent.parent.box.contentWidth), None, None)
    // TODO: Also use fixed when table format style is defined to fixed
    val useFixed = tableWidthOpt.isDefined
    val widths: Vector[Int] = if (useFixed) {
      rowGroups.headOption.flatMap(_.firstRowOption) match {
        case Some(firstRow) => firstRow.findWidths(tableWidthOpt)
        case None => Vector.empty
      }
    } else {
      val allWidths = rowGroups.flatMap { _.findPrefWidths()}
      val maxCols = allWidths.map(_.size).maxOption.getOrElse(0)
      ((0 until maxCols) map (col => allWidths.map(rowWidths => if (col < rowWidths.size) rowWidths(col) else 0).maxOption.getOrElse(0))).toVector
    }
    println(s" widths: " + widths)
    rowGroups.flatMap(_.getRows) foreach {row =>
      val h = row.layout(currY, widths, vwProps)
      currY += h
    }
    rowGroups.foreach(_.finishLayout())
  }
}

object TFCUtil {
  val groupDIs = List("table-row-group", "table-header-group", "table-footer-group")

}

/**
 * This context assumes that content is well structured aforehand, as follows:
 *  - display:table
 *  -   display:table-column-group
 *  -     display:table-column
 *  -   display:table-row-group
 *  -     display:table-row
 *  -     display:table-cell
 **/
final class TableFormattingContext extends FormattingContext {

  override def innerLayout(c: Content, marginCollapseTopAvl: Int, lc: LayoutConstraints): Int = {
    Util.logLayout(1, s"  table inner layout of $c", c.level)

    c.hydrateSimpleProps()

    val mc = new TableFixedLayoutMiniContext(c)
    val subs = c.getSubContent()
    subs foreach (mc.add)

    mc.finishLayout(lc.vwProps)
    c.miniContext = mc
    val mcHeight = mc.getHeight
    c.box.contentHeight = mc.getHeight
    c.box.contentWidth = mc.getWidth

    FCUtil.positionAbsolutes(c, lc.vwProps)

    Util.logLayout(1, s"✓ table inner layout of $c, dim: ${c.box.marginBoxWidth} x ${c.box.marginBoxHeight}", c.level)
    c.box.marginBoxHeight
  }

  override def preferredWidths(c: Content, withMarginPaddingBorder: Boolean): PrefWidths = {
    // TODO: honour `withMarginPaddingBorder` parameter
    val rwOpt = c.resolveLength(c.props.width, Some(c.containingWidth), autoValue = None, noneValue = None)
    val pwResult = rwOpt.map(rw => PrefWidths(rw, rw)).getOrElse({
      val subs = c.getSubContent()
      val useFixedLayout = false  // TODO: check table-layout property
      if (useFixedLayout) {
        val firstRowGroupOpt = subs.find(s => TFCUtil.groupDIs.contains(s.props.displayInner))
        val firstRowOpt = firstRowGroupOpt
                        .map(_.getSubContent().find(_.props.displayInner == "table-row"))
                        .getOrElse(subs.find(_.props.displayInner == "table-row"))

        firstRowOpt.map {firstRow =>
          val firstRowCells = firstRow.getSubContent().filter(_.props.displayInner == "table-cell")
          // val declaredWidths = firstRowCells.map(cell => cell.resolveLength(cell.props.width, None, None, None).map(dw => dw + c.box.borderWidth + c.box.paddingWidth))
          val pws = firstRowCells.map(cell => cell.getFormattingContext().preferredWidths(cell, true))
          val prefMinWidth = pws.map(_.prefMinWidth).sum
          val prefWidth = pws.map(_.prefWidth).sum
          PrefWidths(prefMinWidth, prefWidth)
        } getOrElse (PrefWidths(0, 0))
      } else {
        val rows = subs.flatMap(s => if (TFCUtil.groupDIs.contains(s.props.displayInner)) s.getSubContent else None)
        val allPWs = rows.map { row =>
          val cells = row.getSubContent().filter(_.props.displayInner == "table-cell")
          cells.map(cell => cell.getFormattingContext().preferredWidths(cell, true))
        }
        println(allPWs)
        val maxCols = allPWs.map(_.size).maxOption.getOrElse(0)
        val prefMinWidths = (0 until maxCols) map (col => allPWs.map(rows => if (col < rows.size) rows(col).prefMinWidth else 0).maxOption.getOrElse(0))
        println("pref min widths")
        println(prefMinWidths)
        val prefWidths = (0 until maxCols) map (col => allPWs.map(rows => if (col < rows.size) rows(col).prefWidth else 0).maxOption.getOrElse(0))

        val prefMinWidth = prefMinWidths.sum
        val prefWidth = prefWidths.sum
        println(s"pref min width: $prefMinWidth, prefWidth: $prefWidth")
        PrefWidths(prefMinWidth, prefWidth)
      }
    })
    pwResult
  }

}
