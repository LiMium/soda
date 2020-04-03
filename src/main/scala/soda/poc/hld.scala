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
    val rootBoxOpt = generateBoxElem(rootElem, None, ContainingBlockRef(ContentArea, initialCB))
    rootBoxOpt.foreach {rootBox =>
      initialCB.setRootBox(rootBox)
    }
    rootBoxOpt
  }

  def layoutRoot(rootBoxP: BoxWithProps, vwProps: ViewPortProps):Unit = {
    val initCB = rootBoxP.containingBlock.cb.b
    initCB.contentWidth = vwProps.width
    initCB.contentHeight = vwProps.height

    rootBoxP.formattingContext.foreach(_.layout(vwProps))

    rootBoxP.computeRelativeOffsets(vwProps)
  }

  def separateAbsAndFixed(btn: BoxTreeNode):Unit = {
    if (btn.boxyDomChildren != null) {
      val boxy = btn.boxyDomChildren.collect( { case b:BoxWithProps => b })
      val abs = boxy.filter(c => absolutish.contains(c.positionProp))
      abs.foreach { childBoxP =>
        childBoxP.isInflow = false
        childBoxP.containingBlock.addAsAbsoluteChild(childBoxP)
      }
      btn.boxyDomChildren.foreach {separateAbsAndFixed(_)}
    }
    if (btn.inlinyDomChildren != null) {
      val boxy = btn.inlinyDomChildren.collect( { case b:BoxWithProps => b })
      val abs = boxy.filter(c => absolutish.contains(c.positionProp))
      abs.foreach { childBoxP =>
        childBoxP.isInflow = false;
        childBoxP.containingBlock.addAsAbsoluteChild(childBoxP)
      }
      boxy.foreach (separateAbsAndFixed)
    }
  }

  def createAnonBoxes(boxP: BoxTreeNode):Unit = {
    // TODO
    // boxP.inflowChildren.foreach { createAnonBoxes }
  }

  private def generateBoxElem(en: ElementNode, domParentBox: Option[BoxWithProps], containingBlock: ContainingBlockRef): Option[BoxWithProps] = {
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
      boxWithProps.containingBlock = containingBlock
      val children = en.children.flatMap(generateBoxNode(_, boxWithProps))

      val inlineMode = children.forall( {
        case boxP: BoxWithProps => absolutish.contains(boxP.positionProp) || (boxP.displayOuter == "inline" || boxP.displayOuter == "run-in")
        case ab: AnonInlineBox => true
        case tr: TextRun => true
      })
      if (inlineMode) {
        boxWithProps.inlinyDomChildren = children.map(_.asInstanceOf[InlineSource])
      } else {
        boxWithProps.boxyDomChildren = children.map({
          case btn: BoxTreeNode => btn
          case tRun: TextRun => new AnonInlineBox(tRun, boxWithProps)
        })
      }
      Some(boxWithProps)
    }
  }

  val absolutish = Array("absolute", "fixed")
  val nonStatic = absolutish :+ "relative"

  private def findContainingBlock(positionProp: String, parentBox: BoxWithProps) : ContainingBlockRef = {
    if (positionProp == "absolute") {
      findAbsContainingBlock(parentBox)
    } else if (positionProp == "fixed") {
      // ContainingBlockRef(PaddingArea, findRootBox(parentBox))
      findRootBox(parentBox).containingBlock
    } else {
      ContainingBlockRef(ContentArea, parentBox)
    }
  }

  private def findRootBox(box: BoxWithProps): BoxWithProps = {
    box.domParentBox match {
      case Some(pb) => findRootBox(pb)
      case None => box
    }
  }

  private def findAbsContainingBlock(parentBox: BoxWithProps) : ContainingBlockRef = {
    val parentPos = parentBox.positionProp
    if (parentBox.domParentBox.isEmpty || nonStatic.contains(parentPos)) {
      ContainingBlockRef(PaddingArea, parentBox)
    } else {
      ContainingBlockRef(PaddingArea, parentBox.domParentBox.get)
    }
  }

  private def generateBoxNode(dn: DecoratedNode, parentBox: BoxWithProps): Option[BasicNode] = {
    dn match {
      case en: ElementNode => {
        // val containingBlock = if (absolutish.contains(en.positionProp.get)) findContainingBlock(parentBox) else ContainingBlockRef(ContentArea, parentBox)
        val containingBlock = findContainingBlock(en.positionProp.get, parentBox)
        generateBoxElem(en, Some(parentBox), containingBlock)
      }
      case tn: TextNode => {
        val textRun = new TextRun(tn, parentBox)
        if (parentBox.innerBoxType == InlineBoxType) {
          Some(textRun)
        } else {
          Some(new AnonInlineBox(textRun, parentBox))
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
