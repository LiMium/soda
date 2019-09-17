package soda.poc

import soda.analysis.DecoratedNode
import soda.analysis.DocumentNode
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps

object Layout {
  def process(dn: DocumentNode, vwProps: ViewPortProps): Option[BoxTreeNode] = {
    dn.computeL1Props()
    val rootBoxPOpt = generateBoxes(dn)

    rootBoxPOpt foreach { rootBoxP =>
      separateAbsAndFixed(rootBoxP)
      createAnonBoxes(rootBoxP)

      // println(rootBoxP.dump(0))
      rootBoxP.computeL2Props(vwProps)

      layoutRoot(rootBoxP, vwProps)

      rootBoxP
    }

    rootBoxPOpt
  }

  def generateBoxes(dn: DocumentNode): Option[BoxWithProps] = {
    val rootElem = dn.childElements.find(_.elem.isRootElem).get
    val initialCB = new InitialContainingBlock()
    Util.generateBoxElem(rootElem, None, initialCB)
  }

  def layoutRoot(rootBoxP: BoxWithProps, vwProps: ViewPortProps):Unit = {
    val initCB = rootBoxP.containingBlock.b
    initCB.contentWidth = vwProps.width
    initCB.contentHeight = vwProps.height

    rootBoxP.formattingContext.foreach(_.layout(vwProps))

    rootBoxP.computeRelativeOffsets(vwProps)
  }

  def separateAbsAndFixed(boxP: BoxTreeNode):Unit = {
    // TODO
    // boxP.inflowChildren = boxP.domChildren
    // boxP.domChildren.foreach {separateAbsAndFixed}
  }

  def createAnonBoxes(boxP: BoxTreeNode):Unit = {
    // TODO
    // boxP.inflowChildren.foreach { createAnonBoxes }
  }

  /*
  def generateBoxes(decoratedNode: DecoratedNode): Vector[BoxWithProps] = {
    decoratedNode match {
      case dn: DocumentNode => {
        val box = new Box()
        val bfc = new BlockFormattingContext()
        val boxWithProps = new BoxWithProps(box, Some(dn), None, None, Some(bfc))
        bfc.establishingBox = boxWithProps
        bfc.generateBoxes()
        Vector(boxWithProps)
      }
      case en: ElementNode => {
        val box = new Box()
        val bfc = new BlockFormattingContext()
        val boxWithProps = new BoxWithProps(box, Some(en), None, None, Some(bfc))
        bfc.establishingBox = boxWithProps
        boxWithProps.children = en.children.flatMap(c => generateBoxes(c))
        Vector(boxWithProps)
      }
      case default => Vector.empty
    }

  }
  * 
  */
}
