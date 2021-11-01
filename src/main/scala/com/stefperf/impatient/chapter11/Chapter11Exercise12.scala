package com.stefperf.impatient.chapter11

import com.stefperf.impatient._

import scala.collection.mutable
import scala.language.dynamics


// a better implementation would need to treat sub-elements differently depending on whether they are unique
class XMLElement(val name: String, val text: String = "", val attributes: Map[String, String] = Map.empty,
                 val children: mutable.ArrayBuffer[XMLElement] = mutable.ArrayBuffer.empty) extends Dynamic {

  // this method is needed to fulfill the exercise specs, but it is actually a bad idea, because:
  // 1. it might throw an exception if no child of the given name is found;
  // 2. it cannot handle multiple children with the same name, just returning the first instead
  def selectDynamic(elemName: String): XMLElement = children.find(_.name == elemName).get

  // also not a good idea for the same reasons as above...
  def applyDynamicNamed(elemName: String)(nameValuePairs: (String, String)*): XMLElement = {
    children.find(child =>
      child.name == elemName && nameValuePairs.forall(nameValuePair => {
        val (attrName, value) = nameValuePair
        child.attributes.get(attrName).fold(false)(_ == value)
      })
    ).get
  }

  def li: Seq[XMLElement] = children.filter(_.name == "li")

  override def toString: String =
    s"XMLElement(name='$name', text='$text', ${attributes.size} attributes, ${children.length} children)"
}


object Chapter11Exercise12 extends App {
  val rootElement = new XMLElement("rootElement")
  val html = new XMLElement("html")
  rootElement.children += html
  val body = new XMLElement("body")
  html.children += body
  var liIndex = 0
  for (i <- 41 to 43; id = i.toString; ul = new XMLElement("ul", "ul" + id, Map("id" -> id))) {
    body.children += ul
    for (_ <- 1 to 3) {
      liIndex += 1
      val liId = liIndex.toString
      ul.children += new XMLElement("li", "li" + liId)
    }
  }
  println("(This exercise / example is just didactic and not really a good idea, at least not implemented this way)")
  println("println(rootElement.html.body.ul(id=\"42\").li):")
  println(rootElement.html.body.ul(id="42").li)
}

