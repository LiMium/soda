package soda

import java.awt.Color
import java.awt.Graphics2D
import java.awt.image.BufferedImage

import soda.dom.RenderableDocument
import soda.poc.config

object Renderer {
  def render(url: java.net.URL, userCSS: String) = {
    val width = 800
    val height = 600
    val outImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    val g = outImg.createGraphics()
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, width, height)
    g.clipRect(0, 0, width, height)

    val g2 = g.asInstanceOf[Graphics2D]
    renderNew(g, url, width, height, userCSS)

    if (config.drawGrid) {
      drawGrid(g, config.gridSpacing, width, height)
    }

    g.dispose()

    outImg
  }

  private def drawGrid(g: Graphics2D, spacing: Int, w: Int, h: Int) = {
    g.setColor(new Color(200, 240, 255, 70))
    for (x <- 0 until w by spacing) { g.drawLine(x, 0, x, h) }
    for (y <- 0 until h by spacing) { g.drawLine(0, y, w, y) }

    g.setColor(new Color(200, 240, 255, 120))
    for (x <- 0 until w by spacing*spacing) { g.drawLine(x, 0, x, h) }
    for (y <- 0 until h by spacing*spacing) { g.drawLine(0, y, w, y) }

  }

  def renderNew(g2: Graphics2D, url: java.net.URL, width: Int, height: Int, userCSS: String) = {
    import soda.poc._
    import soda.analysis.Analyser
    import soda.layout.ViewPortProps

    val decoratedDOM = Analyser.process(url)
    val vwProps = ViewPortProps(width, height, 96, g2)
    val rootBoxOpt = Layout.process(decoratedDOM, vwProps)
    rootBoxOpt foreach {r =>
      r.paintAll(g2)
    }
  }
}
