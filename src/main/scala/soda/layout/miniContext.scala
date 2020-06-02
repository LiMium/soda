package soda.layout

import java.awt.Graphics2D
import soda.utils.Util

trait MiniContext[T <: Content] {
  def add(c: T):Unit
  def getHeight: Int
  def paint(g:Graphics2D):Unit
  def getCurrPosXY(): (Int, Int)
  def isNotEmpty: Boolean
}

object EmptyMiniContext extends MiniContext[Content] {
  def add(c: Content):Unit = ???
  def getHeight: Int = 0
  def paint(g:Graphics2D):Unit = {
    // println("Painting of empty minicontext")
  }
  def getCurrPosXY(): (Int, Int) = (0, 0)
  def isNotEmpty: Boolean = false
}
