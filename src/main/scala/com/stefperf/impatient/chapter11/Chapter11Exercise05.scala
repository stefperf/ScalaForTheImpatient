package com.stefperf.impatient.chapter11

import collection.mutable.Queue

sealed abstract class HtmlTag(content: Queue[Either[String, HtmlTag]]) {
  def name: String
  def open: String = s"<$name>"
  def close: String = s"</$name>"
  override def toString: String = s"$open${
    content.map(el => el.fold(x => x, x => x.toString)).mkString
  }$close"
}

final case class Td(content: String = "")
  extends HtmlTag(Queue(Left[String, HtmlTag](content))) {
  override val name: String = "td"
}

final case class Tr(content: Queue[Td] = Queue[Td]())
  extends HtmlTag(content.map(Right[String, Td])) {
  def this(td: Td) = this(Queue(td))
  override val name: String = "tr"
}

final case class Table(content: Queue[Tr] = Queue[Tr]())
  extends HtmlTag(content.map(Right[String, Tr])) {
  override val name: String = "table"
  def |(text: String): Table = {
    val newTd = Td(text)
    if (content.isEmpty)
      copy(Queue(Tr(Queue(newTd))))
    else {
      val lastTr = content.last
      copy(content.init :+ lastTr.copy(lastTr.content :+ newTd))
    }
  }
  def ||(text: String): Table = copy(content :+ Tr(Queue(Td(text))))
}

object Chapter11Exercise05 extends App {
  println("println(Table() | \"Java\" | \"Scala\" || \"Gosling\" | \"Odersky\" || \"JVM\" | \"JVM, .NET\")")
  println(" -> ")
  println(Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET")
}
