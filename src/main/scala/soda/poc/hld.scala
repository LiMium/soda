package soda.poc

import java.net.URL
import javax.imageio.ImageIO

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
    generateBoxElem(rootElem, None, initialCB)
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

  private def generateBoxElem(en: ElementNode, domParentBox: Option[BoxWithProps], containingBlock: HasBox): Option[BoxWithProps] = {
    val displayComputed = en.displayProp.computed
    val floatComputed = en.floatProp.computed
    val positionComputed = en.positionProp.computed
    if (displayComputed == Some("none")) {
      None
    } else {
      val box = new Box()
      if (en.elem.tag == "img") {
        val src = en.elem.getAttribute("src")
        val baseUrl = en.elem.getBaseURI()
        val imgUrl = new URL(new URL(baseUrl), src)
        box.img = ImageIO.read(imgUrl)
      }
      val boxWithProps = new BoxWithProps(box, en, domParentBox)
      boxWithProps.domChildren = en.children.flatMap(generateBoxNode(_, boxWithProps))
      boxWithProps.containingBlock = containingBlock
      Some(boxWithProps)
    }
  }

  private def generateBoxNode(dn: DecoratedNode, parentBox: BoxWithProps): Option[BoxTreeNode] = {
    dn match {
      case en: ElementNode => generateBoxElem(en, Some(parentBox), parentBox)
      case tn: TextNode => {
        val textRun = new TextRun(tn, parentBox)
        if (parentBox.innerBoxType == InlineBoxType) {
          Some(textRun)
        } else {
          Some(new AnonInlineBox(new Box(), textRun, parentBox))
        }
      }
      case _ => ???
    }
  }

  /*
  def adjustBoxes(): Unit = {
    domChildren = if (blockLevel) {
      ensureBlock(domChildren)
    } else {
      ensureInline(domChildren)
    }
  }
  */

  /*
  private def ensureBlock(childBoxes: Vector[BoxTreeNode]) : Vector[BoxTreeNode] = {
    var blocked = Vector[BoxTreeNode]()
    childBoxes.foreach {cb =>
      cb match {
        case boxWithProps : BoxWithProps => {
          if (boxWithProps.blockLevel) {
            blocked :+= boxWithProps
          } else {
            // TODO: combine adjacent inline boxes
            val anonBox = new Box()
            blocked :+= new BoxWithProps(anonBox, None, None, None)
          }
        }
        case _ =>
      }
    }
    blocked
  }

  private def ensureInline(childBoxes: Vector[BoxTreeNode]) : Vector[BoxTreeNode] = {
    var blocked = Vector[BoxTreeNode]()
    childBoxes.foreach { cb =>
      cb match {
        case boxWithProps : BoxWithProps => {
          if (boxWithProps.blockLevel) {
            blocked :+= cb
          } else {
            // TODO: combine adjacent inline boxes
            val anonBox = new Box()
            blocked :+= new BoxWithProps(anonBox, None, None, None)
          }
        }
        case _ =>
      }
    }
    blocked
  }
  */


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
