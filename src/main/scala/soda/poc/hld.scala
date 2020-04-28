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
      // println(rootBoxP.dump(0))
      rootBoxP.computeL2Props(vwProps)

      val content = layoutRoot(rootBoxP, vwProps)

      content
    }

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
    val initFCOrig = new FormattingContext {
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
    */

    // We create an initial containing block with dimensions equal to viewport and position: relative
    // This simplifies the computation of containing block to be a simpler recursive process
    val initCB = new BlockContent(null, None, "initial cb", new RenderProps(null, "visible", "visible", true)) {
      def getFormattingContext(): soda.poc.FormattingContext = new FlowFormattingContext(null)
      def getSubContent(): Vector[soda.poc.Content] = rootBoxP.getContents(this, vwProps)
      val props: soda.poc.LayoutProps = new LayoutProps(
        "block", "flow-root", "relative",
        ContentUtil.emptyOffsets, ContentUtil.emptyBorder, ContentUtil.emptyOffsets,
        NoneLength, NoneLength, NoneLength, NoneLength, ContentUtil.emptyFontProp, ContentUtil.emptyOffsets)
    }
    initCB.box.contentWidth = vwProps.width
    initCB.box.contentHeight = vwProps.height

    val constraints = LayoutConstraints(FitAvailable(vwProps.width), FitToShrink(vwProps.height), vwProps)
    initCB.getFormattingContext().innerLayout(initCB, constraints)

    initCB
  }

  private def generateBoxElem(en: ElementNode, domParentBox: Option[BoxWithProps] /*, containingBlock: ContainingBlockRef*/): Option[BoxWithProps] = {
    val displayComputed = en.displayProp.computed
    val floatComputed = en.floatProp.computed
    val positionComputed = en.positionProp.computed
    if (displayComputed == Some("none")) {
      None
    } else {
      val boxWithProps = new BoxWithProps(en, domParentBox)
      val children = en.children.flatMap(generateBoxNode(_, boxWithProps))

      boxWithProps.boxyDomChildren = children
      Some(boxWithProps)
    }
  }

  val absolutish = Array("absolute", "fixed")
  val nonStatic = absolutish :+ "relative"

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

}
