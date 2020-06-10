package soda.layout

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

  def isNotEmpty = child != null
}

object Layout {
  def process(dn: DocumentNode, vwProps: ViewPortProps): Option[Content] = {
    dn.computeL1Props()
    val rootBoxPOpt = generateBoxes(dn)

    val contentOpt = rootBoxPOpt map { rootBoxP =>
      rootBoxP.adjustChildren()
      // println(rootBoxP.dump(0))
      rootBoxP.computeL2Props(vwProps)

      val content = layoutRoot(rootBoxP, vwProps)

      content
    }

    contentOpt
  }

  def generateBoxes(dn: DocumentNode): Option[BoxWithProps] = {
    val rootElem = dn.childElements.find(_.elem.isRootElem).get
    val rootBoxOpt = generateBoxElem(rootElem, None)
    rootBoxOpt
  }

  def layoutRoot(rootBoxP: BoxWithProps, vwProps: ViewPortProps):Content = {
    // We create an initial containing block with dimensions equal to viewport and position: relative
    // This simplifies the computation of containing block to be a simpler recursive process
    val initCB = new BlockContent(null, None, "initial cb", new RenderProps(null, "visible", "visible", true)) {
      def getFormattingContext(): soda.layout.FormattingContext = new FlowFormattingContext
      def getSubContent(): Vector[soda.layout.Content] = rootBoxP.getContents(this, vwProps)
      val props: soda.layout.LayoutProps = new LayoutProps(
        "block", "flow-root", "relative",
        ContentUtil.emptyOffsets, ContentUtil.emptyBorder, ContentUtil.emptyOffsets,
        NoneLength, NoneLength, NoneLength, NoneLength, ContentUtil.emptyFontProp, ContentUtil.emptyOffsets)
    }
    initCB.box.contentWidth = vwProps.width
    initCB.box.contentHeight = vwProps.height

    val constraints = LayoutConstraints(FitAvailable(vwProps.width), FitToShrink(vwProps.height), vwProps)
    initCB.getFormattingContext().innerLayout(initCB, 0, constraints)

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

      boxWithProps.setBoxyDomChildren(children)
      Some(boxWithProps)
    }
  }

  private def generateBoxNode(dn: DecoratedNode, parentBox: BoxWithProps): Option[BoxTreeNode] = {
    dn match {
      case en: ElementNode => {
        generateBoxElem(en, Some(parentBox))
      }
      case tn: TextNode => {
        Some(new AnonBox(tn, parentBox))
      }
      case _ => ???
    }
  }

}
