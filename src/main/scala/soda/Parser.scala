package soda

import java.io.FileInputStream
import java.io.InputStream

import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

import javax.xml.parsers.SAXParserFactory
import soda.dom.RenderableDocument
import soda.dom.RenderableElement
import soda.dom.RenderableNode
import soda.dom.RenderableText

object Parser {
  def parse(file: java.io.File):RenderableDocument = {
    val is = new FileInputStream(file)
    parse(file.toURI().toString(), is)
  }

  def parse(uri: String, is: InputStream):RenderableDocument = {
    val factory:SAXParserFactory = SAXParserFactory.newInstance()
    factory.setValidating(false)
    val sparser = factory.newSAXParser()
    val reader = sparser.getXMLReader
    reader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)

    val renderableHandler = new DefaultHandler {
      val doc = new RenderableDocument("", uri)
      var currentNode: RenderableNode = doc
      override def startDocument = { /* println("Doc started") */ }
      override def startElement(uri:String, localName:String, qName:String, attributes:Attributes) = {
        // println(s"start element: uri: $uri  localName: $localName  qName: $qName attributes:$attributes")
        val isRootElem = currentNode eq doc
        if (isRootElem) {
          assert(currentNode.children.size == 0)
        }

        val nn = new RenderableElement(doc, qName, mkAttributeMap(attributes), isRootElem)
        currentNode.appendChild(nn)
        currentNode = nn
      }
      override def endElement(uri: String, localName: String, qName: String) = {
        currentNode = currentNode.getParentNode.asInstanceOf[RenderableNode]
      }
      override def characters(ch: Array[Char], start:Int, length:Int) = {
        val str = ch.subSequence(start, start+length).toString
        if (str.length > 0) {
          val nn = new RenderableText(str)
          currentNode.appendChild(nn)
        }
      }

      override def endDocument = { /* println("Doc ended") */}
    }
    try {
      sparser.parse(is, renderableHandler)
    } catch {
      case ex: org.xml.sax.SAXParseException =>
        val errorStr = s"<html><body><p>Encountered error while parsing</p><p>${ex}</p></body></html>"
        val errorStream = new java.io.ByteArrayInputStream(errorStr.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        sparser.parse(errorStream, renderableHandler)
    }
    renderableHandler.doc
  }

  def mkAttributeMap(attributes: Attributes) = {
    val m = collection.mutable.Map[String, String]()
    for (i <- 0 until attributes.getLength) {
      val key = attributes.getLocalName(i)
      val value = attributes.getValue(i)
      m(key) = value
    }
    m.toMap
  }
}
