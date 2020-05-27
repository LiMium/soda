package soda.poc

import java.awt.Color

object config {
  val layoutDebugLevel = 0
  val paintDebugLevel = 0
  val lineDebugColor = Color.CYAN

  val showBoxes = false

  val drawGrid = false
  val gridSpacing = 16

  val affectsRender = paintDebugLevel > 0 || showBoxes || drawGrid
}
