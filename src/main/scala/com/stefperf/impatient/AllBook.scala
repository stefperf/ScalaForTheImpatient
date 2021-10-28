package com.stefperf.impatient

import com.stefperf.impatient.chapter01.Chapter01
import com.stefperf.impatient.chapter02.Chapter02
import com.stefperf.impatient.chapter03.Chapter03
import com.stefperf.impatient.chapter04.Chapter04
import com.stefperf.impatient.chapter05.Chapter05
import com.stefperf.impatient.chapter06.Chapter06
import com.stefperf.impatient.chapter07.Chapter07
import com.stefperf.impatient.chapter08.Chapter08
import com.stefperf.impatient.chapter09.Chapter09
import com.stefperf.impatient.chapter10.Chapter10
import com.stefperf.impatient.chapter11.Chapter11

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

//execute only the chapters requested as command line arguments, or all chapters sequentially if no argument is given
object AllBook extends App {
  // force all objects to be initialized and available
  Chapter01.forceInit()
  Chapter02.forceInit()
  Chapter03.forceInit()
  Chapter04.forceInit()
  Chapter05.forceInit()
  Chapter06.forceInit()
  Chapter07.forceInit()
  Chapter08.forceInit()
  Chapter09.forceInit()
  Chapter10.forceInit()
  Chapter11.forceInit()

  val requestedChapters = ArrayBuffer[Int]()
  breakable {
    for (arg <- args) {
      var num = -1
      try {
        num = arg.toInt
      }
      catch {
        case _: Throwable =>
          println(f"Arguments must be either empty or all integers; '$arg' is not an integer.")
          break
      }
      requestedChapters += num
    }
    Chapter.execute(requestedChapters.toArray: _*)
  }
}
