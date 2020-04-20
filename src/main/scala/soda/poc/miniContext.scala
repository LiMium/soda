package soda.poc

import java.awt.Graphics2D
import soda.utils.Util

trait MiniContext[T <: Content] extends CanPaint {
  def add(c: T):Unit
  def getHeight: Int
  def paint(g:Graphics2D):Unit
  def getCurrPosXY(): (Int, Int)
}

object EmptyMiniContext extends MiniContext[Content] {
  def add(c: Content):Unit = ???
  def getHeight: Int = 0
  def paint(g:Graphics2D):Unit = {
    // println("Painting of empty minicontext")
  }
  def getCurrPosXY(): (Int, Int) = (0, 0)
}
