package soda.utils

import soda.Parser
import java.net.URL
import soda.layout.config

case class PositionedElement[T](elem: T, isFirst: Boolean, isLast: Boolean, index: Int)

case class Positioned[T](iter: Iterator[T]) extends Iterator[PositionedElement[T]] {
  private var index = 0
  def hasNext = iter.hasNext
  def next = {
    val e = iter.next
    val pe = PositionedElement(e, index == 0, !hasNext, index)
    index += 1
    pe
  }
}

object Util {
  def parse(url: URL) = {
    val stream = url.openStream()
    Parser.parse(url.toExternalForm(), stream)
  }

  def warnln(s: String) = {
    println(Console.RED_B + s + Console.RESET)
  }

  def logLayout(logLevel: Int, s: String, indentLevel: Int = 0): Unit = {
    if (logLevel <= config.layoutDebugLevel) {
      println(("."*indentLevel) + s)
    }
  }
}