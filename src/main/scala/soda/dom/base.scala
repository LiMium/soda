package soda.dom

import soda.layout.Renderable
import soda.layout.RBox
import soda.layout.RWord
import org.w3c.dom.Node
import org.w3c.dom.Element
import org.w3c.dom.Text
import org.w3c.dom.Document
import cz.vutbr.web.domassign.StyleMap
import soda.layout.RDocument
import soda.utils.Positioned
import org.w3c.dom.NodeList
import soda.layout.Renderable
import soda.layout.RImage
import javax.imageio.ImageIO
import java.net.URL
import cz.vutbr.web.css.StyleSheet
import cz.vutbr.web.domassign.AnalyzerUtil
import cz.vutbr.web.css.MediaSpec
import soda.layout.DomRenderable

class NodeListImpl(seq: Vector[Node]) extends NodeList {
  def item(index: Int) = seq(index)
  def getLength(): Int = seq.length
}

sealed trait RenderableNode extends Node {
  val children = collection.mutable.ArrayDeque[RenderableNode]()
  def childElements = children.collect { case re : RenderableElement => re }

  var parentNode: RenderableNode = null
  val ownerDoc: Document

  def getReplaceableRenderables(parent: DomRenderable): Seq[Renderable]

  def positionedChildren = Positioned(children.iterator)

  def walkAllElements(): Vector[RenderableElement] = {
    val elemChildren = children.flatMap({case re: RenderableElement => Some(re); case _ => None}).toVector
    elemChildren ++ elemChildren.flatMap(e => e.walkAllElements)
  }

  // Members declared in org.w3c.dom.Node
  def appendChild(child: org.w3c.dom.Node): org.w3c.dom.Node = {
    child match {
      case crn: RenderableNode =>
        children += crn
        crn.parentNode = this
        crn
    }
  }
  def cloneNode(x$1: Boolean): org.w3c.dom.Node = ???
  def compareDocumentPosition(x$1: org.w3c.dom.Node): Short = ???
  def getAttributes(): org.w3c.dom.NamedNodeMap = ???
  def getBaseURI(): String = getOwnerDocument().getDocumentURI
  def getChildNodes(): org.w3c.dom.NodeList = ???
  def getFeature(x$1: String, x$2: String): Object = ???
  def getFirstChild(): org.w3c.dom.Node = children.headOption.getOrElse(null)
  def getLastChild(): org.w3c.dom.Node = ???
  def getLocalName(): String = ???
  def getNamespaceURI(): String = ???

  def getPrevSiblingOf(n: RenderableNode) = {
    val index = children.indexOf(n)
    if (index > 0) {
      children(index - 1)
    } else {
      null
    }
  }

  def getSiblingOf(n: RenderableNode) = {
    val index = children.indexOf(n)
    if (children.length > index + 1) {
      children(index + 1)
    } else {
      null
    }
  }

  def getNextSibling(): org.w3c.dom.Node = {
    parentNode.getSiblingOf(this)
  }

  def getNodeValue(): String = ???
  def getOwnerDocument(): org.w3c.dom.Document = ownerDoc
  def getParentNode(): org.w3c.dom.Node = parentNode
  def getPrefix(): String = ???
  def getPreviousSibling(): org.w3c.dom.Node = {
    parentNode.getPrevSiblingOf(this)
  }
  def getTextContent(): String = {
    children.map(_.getTextContent).mkString
  }

  def getUserData(x$1: String): Object = ???
  def hasAttributes(): Boolean = ???
  def hasChildNodes(): Boolean = !children.isEmpty
  def insertBefore(x$1: org.w3c.dom.Node, x$2: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def isDefaultNamespace(x$1: String): Boolean = ???
  def isEqualNode(x$1: org.w3c.dom.Node): Boolean = ???
  def isSameNode(x$1: org.w3c.dom.Node): Boolean = ???
  def isSupported(x$1: String, x$2: String): Boolean = ???
  def lookupNamespaceURI(x$1: String): String = ???
  def lookupPrefix(x$1: String): String = ???
  def normalize(): Unit = ???
  def removeChild(x$1: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def replaceChild(x$1: org.w3c.dom.Node, x$2: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def setNodeValue(x$1: String): Unit = ???
  def setPrefix(x$1: String): Unit = ???
  def setTextContent(x$1: String): Unit = ???
  def setUserData(x$1: String, x$2: Any, x$3: org.w3c.dom.UserDataHandler): Object = ???

}

object RenderableDocument {
  def apply(uri: String, a_tag: String, a_children: RenderableNode*) = {
    val rd = new RenderableDocument(a_tag, uri)
    a_children.foreach { rd.appendChild(_) }
    rd
  }
}

class RenderableDocument(tag: String, uri: String) extends Document with RenderableNode {
  val ownerDoc = this

  def getReplaceableRenderables(parent: DomRenderable): Seq[Renderable] = Seq.empty

  def prepareRenderTree(styleSheets: java.util.List[StyleSheet]): Renderable = {
    val classifiedRules = AnalyzerUtil.getClassifiedRules(styleSheets, new MediaSpec("screen"))
    val result = new RDocument()
    children.foreach { c =>
      result.addNode(c, classifiedRules, None)
    }
    result
  }

  // Members declared in org.w3c.dom.Document
  def adoptNode(x$1: org.w3c.dom.Node): org.w3c.dom.Node = ???
  def createAttribute(x$1: String): org.w3c.dom.Attr = ???
  def createAttributeNS(x$1: String, x$2: String): org.w3c.dom.Attr = ???
  def createCDATASection(x$1: String): org.w3c.dom.CDATASection = ???
  def createComment(x$1: String): org.w3c.dom.Comment = ???
  def createDocumentFragment(): org.w3c.dom.DocumentFragment = ???
  def createElement(x$1: String): org.w3c.dom.Element = ???
  def createElementNS(x$1: String, x$2: String): org.w3c.dom.Element = ???
  def createEntityReference(x$1: String): org.w3c.dom.EntityReference = ???
  def createProcessingInstruction(x$1: String, x$2: String): org.w3c.dom.ProcessingInstruction = ???
  def createTextNode(x$1: String): org.w3c.dom.Text = ???
  def getDoctype(): org.w3c.dom.DocumentType = ???
  def getDocumentElement(): org.w3c.dom.Element = children.headOption.map(_.asInstanceOf[Element]).getOrElse(null)
  def getDocumentURI(): String = uri
  def getDomConfig(): org.w3c.dom.DOMConfiguration = ???
  def getElementById(x$1: String): org.w3c.dom.Element = ???
  def getElementsByTagName(name: String): org.w3c.dom.NodeList = {
    new NodeListImpl(walkAllElements.filter(_.getNodeName() == name))
  }

  def getElementsByTagNameNS(x$1: String, x$2: String): org.w3c.dom.NodeList = ???
  def getImplementation(): org.w3c.dom.DOMImplementation = ???
  def getInputEncoding(): String = ???
  def getStrictErrorChecking(): Boolean = ???
  def getXmlEncoding(): String = ???
  def getXmlStandalone(): Boolean = ???
  def getXmlVersion(): String = ???
  def importNode(x$1: org.w3c.dom.Node, x$2: Boolean): org.w3c.dom.Node = ???
  def normalizeDocument(): Unit = ???
  def renameNode(x$1: org.w3c.dom.Node, x$2: String, x$3: String): org.w3c.dom.Node = ???
  def setDocumentURI(x$1: String): Unit = ???
  def setStrictErrorChecking(x$1: Boolean): Unit = ???
  def setXmlStandalone(x$1: Boolean): Unit = ???
  def setXmlVersion(x$1: String): Unit = ???

  def getNodeName(): String = tag
  def getNodeType(): Short = Node.DOCUMENT_NODE

}

object RenderableElement {
  def apply(ownerDoc: Document, a_tag: String, a_attributes: Map[String, String], a_isRootElem: Boolean, a_children: RenderableNode*) = {
    val re = new RenderableElement(ownerDoc, a_tag, a_attributes, a_isRootElem)
    a_children.foreach { re.appendChild(_) }
    re
  }
}

class RenderableElement(val ownerDoc: Document, val tag: String, attributes: Map[String, String], val isRootElem: Boolean) extends Element with RenderableNode {
  def getReplaceableRenderables(parent: DomRenderable): Seq[Renderable] = {
    if (tag == "img") {
      attributes.get("src").flatMap{src =>
        val baseUrl = getBaseURI()
        val imgUrl = new URL(new URL(baseUrl), src)
        val img = ImageIO.read(imgUrl)
        if (img != null) {
          Some(new RImage(parent, this, img))
        } else {
          None
        }
      }.toSeq
    } else {
      Seq.empty
    }
  }

  // Members declared in org.w3c.dom.Element
  def getAttribute(name: String): String = attributes.getOrElse(name, null)
  def getAttributeNS(x$1: String, x$2: String): String = ???

  def getAttributeNode(name: String): org.w3c.dom.Attr = {
    val attrib = attributes.getOrElse(name, null)
    if (attrib == null) {
      null
    } else {
      new RenderableAttribute(ownerDoc, name, attrib)
    }
  }

  def getAttributeNodeNS(x$1: String, x$2: String): org.w3c.dom.Attr = ???

  def getElementsByTagName(x$1: String): org.w3c.dom.NodeList = ???
  def getElementsByTagNameNS(x$1: String, x$2: String): org.w3c.dom.NodeList = ???
  def getSchemaTypeInfo(): org.w3c.dom.TypeInfo = ???
  def getTagName(): String = tag
  def hasAttribute(name: String): Boolean = attributes.isDefinedAt(name)
  def hasAttributeNS(x$1: String, x$2: String): Boolean = false
  def removeAttribute(x$1: String): Unit = ???
  def removeAttributeNS(x$1: String, x$2: String): Unit = ???
  def removeAttributeNode(x$1: org.w3c.dom.Attr): org.w3c.dom.Attr = ???
  def setAttribute(x$1: String, x$2: String): Unit = ???
  def setAttributeNS(x$1: String, x$2: String, x$3: String): Unit = ???
  def setAttributeNode(x$1: org.w3c.dom.Attr): org.w3c.dom.Attr = ???
  def setAttributeNodeNS(x$1: org.w3c.dom.Attr): org.w3c.dom.Attr = ???
  def setIdAttribute(x$1: String, x$2: Boolean): Unit = ???
  def setIdAttributeNS(x$1: String, x$2: String, x$3: Boolean): Unit = ???
  def setIdAttributeNode(x$1: org.w3c.dom.Attr, x$2: Boolean): Unit = ???

  def getNodeName(): String = tag
  def getNodeType(): Short = Node.ELEMENT_NODE

  override def toString = {
    tag + "\n" + attributes.mkString(", ") + children.map(_.toString).mkString("\n")
  }
}

class RenderableText(val text: String) extends Text with RenderableNode {
  val ownerDoc = null

  def getReplaceableRenderables(parent: DomRenderable): Seq[Renderable] = Seq.empty

  override def toString = {
    text
  }

  override def getTextContent: String = {
    text
  }

  // Members declared in org.w3c.dom.CharacterData
  def appendData(x$1: String): Unit = ???
  def deleteData(x$1: Int, x$2: Int): Unit = ???
  def getData(): String = ???
  def getLength(): Int = ???
  def insertData(x$1: Int, x$2: String): Unit = ???
  def replaceData(x$1: Int, x$2: Int, x$3: String): Unit = ???
  def setData(x$1: String): Unit = ???
  def substringData(x$1: Int, x$2: Int): String = ???

  // Members declared in org.w3c.dom.Text
  def getWholeText(): String = ???
  def isElementContentWhitespace(): Boolean = ???
  def replaceWholeText(x$1: String): org.w3c.dom.Text = ???
  def splitText(x$1: Int): org.w3c.dom.Text = ???

  def getNodeName(): String = null
  def getNodeType(): Short = Node.TEXT_NODE
}

class RenderableAttribute(val ownerDoc: Document, name: String, value: String) extends org.w3c.dom.Attr with RenderableNode {
  override def getNodeValue(): String = value

  // Members declared in org.w3c.dom.Attr
  def getName(): String = name
  def getOwnerElement(): org.w3c.dom.Element = ???
  def getSchemaTypeInfo(): org.w3c.dom.TypeInfo = ???
  def getSpecified(): Boolean = ???
  def getValue(): String = value
  def isId(): Boolean = ???
  def setValue(x$1: String): Unit = ???

  // Members declared in org.w3c.dom.Node
  def getNodeName(): String = ???
  def getNodeType(): Short = ???

  // Members declared in soda.dom.RenderableNode
  def getReplaceableRenderables(parent: soda.layout.DomRenderable): Seq[soda.layout.Renderable] = ???
}
