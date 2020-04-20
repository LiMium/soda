package soda.poc

import java.net.URL
import javax.imageio.ImageIO

import soda.analysis.DecoratedNode
import soda.analysis.DocumentNode
import soda.analysis.ElementNode
import soda.analysis.TextNode
import soda.layout.ViewPortProps
import java.awt.Graphics2D

class InitialMiniContext extends MiniContext[Content] {
  // Expecting only one child in the initial cb
  private var child: Content = null

  override def add(c: Content): Unit = {child = c}
  override def getHeight: Int = {child.box.marginBoxHeight}
  override def paint(g: Graphics2D): Unit = {
    child.paintAll(g)
  }
  override def getCurrPosXY(): (Int, Int) = (0, 0)
}

object Layout {
  def process(dn: DocumentNode, vwProps: ViewPortProps): Option[Content] = {
    dn.computeL1Props()
    val rootBoxPOpt = generateBoxes(dn)

    val contentOpt = rootBoxPOpt map { rootBoxP =>
      // separateAbsAndFixed(rootBoxP)
      createAnonBoxes(rootBoxP)

      // println(rootBoxP.dump(0))
      rootBoxP.computeL2Props(vwProps)

      val content = layoutRoot(rootBoxP, vwProps)

      // rootBoxP
      content
    }

    // rootBoxPOpt
    contentOpt
  }

  def generateBoxes(dn: DocumentNode): Option[BoxWithProps] = {
    val rootElem = dn.childElements.find(_.elem.isRootElem).get
    // val initialCB = new InitialContainingBlock()
    val rootBoxOpt = generateBoxElem(rootElem, None /*,ContainingBlockRef(ContentArea initialCB )*/)
    /*
    rootBoxOpt.foreach {rootBox =>
      initialCB.setRootBox(rootBox)
    }
    */
    rootBoxOpt
  }

  def layoutRoot(rootBoxP: BoxWithProps, vwProps: ViewPortProps):Content = {
    /*
    val initCB = rootBoxP.containingBlock.cb.b
    initCB.contentWidth = vwProps.width
    initCB.contentHeight = vwProps.height
    */

    // We create an initial containing block with dimensions equal to viewport and position: relative
    // This simplifies the computation of containing block to be a simpler recursive process
    val initCB = new BlockContent(null, None, "initial cb", new RenderProps(null, "visible", "visible")) {
      def getFormattingContext(): soda.poc.FormattingContext = new FormattingContext {
        override def innerLayout(c: Content, lc: LayoutConstraints): Unit = {
          val mc = new InitialMiniContext()
          c.getSubContent().foreach { ch =>
            mc.add(ch)
            ch.box.offsetX = 0
            ch.box.offsetY = 0
            ch.getFormattingContext().innerLayout(ch, lc)
          }
          c.miniContext = mc
          c.absolutes.foreach {abs =>
            val absLC = new LayoutConstraints(FitToShrink(abs.containingWidth), FitToShrink(abs.containingHeight), lc.vwProps)
            abs.getFormattingContext().innerLayout(abs, absLC)
          }
        }
      }
      def getSubContent(): Vector[soda.poc.Content] = rootBoxP.getContents(this, vwProps)
      val props: soda.poc.LayoutProps = new LayoutProps(
        "block", "flow-root", "relative",
        ContentUtil.emptyOffsets, ContentUtil.emptyBorder, ContentUtil.emptyOffsets,
        NoneLength, NoneLength, NoneLength, NoneLength, ContentUtil.emptyOffsets)
    }
    initCB.box.contentWidth = vwProps.width
    initCB.box.contentHeight = vwProps.height

    // val rootContent = rootBoxP.getContents(initCB, vwProps)(0)
    // println(rootContent)
    val constraints = LayoutConstraints(FitAvailable(vwProps.width), FitToShrink(vwProps.height), vwProps)
    // rootBoxP.formattingContext.foreach(_.innerLayout(rootContent, constraints))
    initCB.getFormattingContext().innerLayout(initCB, constraints)

    rootBoxP.computeRelativeOffsets(vwProps)
    // rootContent
    initCB
    // initCB
  }

  /*
  def separateAbsAndFixed(btn: BoxTreeNode):Unit = {
    {
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
  */

  def createAnonBoxes(boxP: BoxTreeNode):Unit = {
    // TODO
    // boxP.inflowChildren.foreach { createAnonBoxes }
  }

  private def generateBoxElem(en: ElementNode, domParentBox: Option[BoxWithProps] /*, containingBlock: ContainingBlockRef*/): Option[BoxWithProps] = {
    val displayComputed = en.displayProp.computed
    val floatComputed = en.floatProp.computed
    val positionComputed = en.positionProp.computed
    if (displayComputed == Some("none")) {
      None
    } else {
      val box = new Box()
      /*
      if (en.elem.tag == "img") {
        val src = en.elem.getAttribute("src")
        val baseUrl = en.elem.getBaseURI()
        val imgUrl = new URL(new URL(baseUrl), src)
        box.img = ImageIO.read(imgUrl)
      }
      */
      val boxWithProps = new BoxWithProps(box, en, domParentBox)
      // boxWithProps.containingBlock = containingBlock
      val children = en.children.flatMap(generateBoxNode(_, boxWithProps))

      val inlineMode = children.forall( {
        case boxP: BoxWithProps => absolutish.contains(boxP.positionProp) || (boxP.displayOuter == "inline" || boxP.displayOuter == "run-in")
        case ab: AnonBox => true
      })
      if (inlineMode) {
        // boxWithProps.inlinyDomChildren = children
        boxWithProps.boxyDomChildren = children
      } else {
        boxWithProps.boxyDomChildren = children
      }
      Some(boxWithProps)
    }
  }

  val absolutish = Array("absolute", "fixed")
  val nonStatic = absolutish :+ "relative"

  /*
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
  */

  /*
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
  */

  private def generateBoxNode(dn: DecoratedNode, parentBox: BoxWithProps): Option[BoxTreeNode] = {
    dn match {
      case en: ElementNode => {
        // val containingBlock = if (absolutish.contains(en.positionProp.get)) findContainingBlock(parentBox) else ContainingBlockRef(ContentArea, parentBox)
        // val containingBlock = findContainingBlock(en.positionProp.get, parentBox)
        generateBoxElem(en, Some(parentBox) /*, containingBlock*/)
      }
      case tn: TextNode => {
        Some(new AnonBox(tn, parentBox))
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
