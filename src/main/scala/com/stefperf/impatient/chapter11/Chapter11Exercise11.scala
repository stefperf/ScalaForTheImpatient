package com.stefperf.impatient.chapter11

import scala.collection.JavaConverters._
import scala.language.dynamics


class DynamicProperties(protected val properties: collection.Map[String, String]) extends Dynamic {
  def selectDynamic(propertyName: String): String = s"${properties.getOrElse(propertyName, "<<<UNDEFINED>>>")}"
}

object SysProps extends DynamicProperties(System.getProperties.asScala) {
  def java: DynamicProperties = new DynamicProperties(properties) {
    override def selectDynamic(propertyName: String): String = super.selectDynamic(s"java.$propertyName")
  }
}


class TieredProperties(private val properties: collection.Map[String, String], private val tier: String = "")
  extends Dynamic {
  def value: Option[String] = properties.get(tier)

  override def toString: String = s"${value.getOrElse("<<<UNDEFINED>>>")}"

  def selectDynamic(propertyName: String): TieredProperties = {
    new TieredProperties(properties, (if (tier.isEmpty) "" else s"$tier.") + propertyName)
  }
}

object TieredSysProps extends TieredProperties(System.getProperties.asScala)


object Chapter11Exercise11 extends App {
  println("-- Solution 1, predefining (=hardcoding) property path 'java.' as suggested: --")
  println("println(SysProps.java.home):")
  println(SysProps.java.home)
  println("println(TieredSysProps.java.vm.specification.version):")
  println("<<<NOT POSSIBLE, BECAUSE THIS PATH IS NOT HARDCODED>>>")
  println()
  println("-- Solution 2, allowing any tiered property path: --")
  println("println(TieredSysProps.java.home):")
  println(TieredSysProps.java.home)
  println("println(TieredSysProps.java.vm.specification.version):")
  println(TieredSysProps.java.vm.specification.version)
}
