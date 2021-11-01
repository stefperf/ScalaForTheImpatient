package com.stefperf.impatient.chapter11

import scala.language.dynamics

class DynamicProperties(properties: collection.Map[String, String]) extends Dynamic {

}

import collection.JavaConverters._
object SysProps extends DynamicProperties(System.getProperties().asScala)

object Chapter11Exercise11 extends App {
  println(SysProps)
}
