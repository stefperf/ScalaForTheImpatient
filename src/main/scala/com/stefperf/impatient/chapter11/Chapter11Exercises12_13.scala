package com.stefperf.impatient.chapter11

import com.stefperf.impatient._

import scala.collection.mutable
import scala.language.dynamics


// a better implementation would need to treat sub-elements differently depending on whether they are unique
class XMLElement(val name: String, val text: String = "", val attributes: Map[String, String] = Map.empty,
                 protected val _children: mutable.ArrayBuffer[XMLElement] = mutable.ArrayBuffer.empty) extends Dynamic {
  import XMLElement._

  def children: Array[XMLElement] = _children.toArray

  // derived classes could override this method to implement schema validation
  def addChildren(els: XMLElement*): Unit = _children ++= els

  // this method is needed to fulfill the exercise specs, but it is actually a bad idea, because:
  // 1. it might throw an exception if no child of the given name is found;
  // 2. it cannot handle multiple _children with the same name, just returning the first instead
  def selectDynamic(elemName: String): XMLElement = _children.find(_.name == elemName).get

  // also not a good idea for the same reasons as above...
  def applyDynamicNamed(elemName: String)(nameValuePairs: (String, String)*): XMLElement = {
    _children.find(child =>
      child.name == elemName && nameValuePairs.forall(nameValuePair =>
        child.attributes.get(nameValuePair._1).fold(false)(_ == nameValuePair._2)
      )
    ).get
  }

  // needed to enable returning multiple elements of this name
  def li: Seq[XMLElement] = _children.filter(_.name == "li")

  override def toString: String =
    s"XMLElement(name='$name', text='$text', ${attributes.size} attributes, ${_children.length} _children)"

  protected def toXML(nestingLevel: Int): String = {
    val indent = INDENT * nestingLevel
    val attributeString: String = attributes.map {
      case (name, value) => s"$name=$QUOTE$value$QUOTE"
    }.mkString(" ")
    val header = s"<$name" + (if (attributes.isEmpty) "" else s" $attributeString") + ">"
    val textContent = if (text.isBlank) "" else text
    val footer = s"</$name>"
    if (_children.isEmpty)
      indent + header + textContent + footer + "\n"
    else
      indent + header + textContent + "\n" + _children.map(_.toXML(nestingLevel + 1)).mkString + indent + footer + "\n"
  }

  def toXML: String = toXML(0)
}


object XMLElement {
  val INDENT = "  "
  val QUOTE = '\"'
}


class XMLBuilder extends Dynamic {
  def applyDynamicNamed(elemName: String)(nameValuePairs: (String, String)*): XMLElement = {
    var text = ""
    val attributes = mutable.Map[String, String]()
    for (((argName, argValue), i) <- nameValuePairs.zipWithIndex) {
      if (i == 0 && argName == "" || argName == "text") text = argValue
      else attributes += argName -> argValue
    }
    new XMLElement(elemName, text, attributes.toMap)
  }

  def applyDynamic(elemName: String)(text: String = ""): XMLElement = new XMLElement(elemName, text)
}


object Chapter11Exercises12_13 extends App {
  println("(This exercise is a simplistic implementation for didactic purposes)")
  val builder = new XMLBuilder()
  val rootElement = builder.rootElement()
  val html = builder.html()
  rootElement.addChildren(html)
  val body = builder.body()
  html.addChildren(body)
  var liIndex = 0
  for (i <- 41 to 43; idStr = i.toString; ul = builder.ul("ul" + idStr, id = idStr)) {
    body.addChildren(ul)
    for (_ <- 1 to 3) {
      liIndex += 1
      val liIdidStr = liIndex.toString
      ul.addChildren(builder.li(liIdidStr))
    }
  }
  println("XML:")
  println(rootElement.toXML)
  println("println(rootElement.html.body.ul(id=\"42\").li):")
  println(rootElement.html.body.ul(id = "42").li)
  println()
  println("println(builder.ul(id=\"42\", style=\"list-style: lower-alpha;\"))")
  println(builder.ul(id="42", style="list-style: lower-alpha;"))
}
