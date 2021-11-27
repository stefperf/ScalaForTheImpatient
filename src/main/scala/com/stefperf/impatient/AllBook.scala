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
import com.stefperf.impatient.chapter12.Chapter12
import com.stefperf.impatient.chapter13.Chapter13
import com.stefperf.impatient.chapter14.Chapter14
// import com.stefperf.impatient.chapter15.Chapter15
// import com.stefperf.impatient.chapter16.Chapter16
import com.stefperf.impatient.chapter17.Chapter17
import com.stefperf.impatient.chapter18.Chapter18
// import com.stefperf.impatient.chapter19.Chapter19
// import com.stefperf.impatient.chapter20.Chapter20
// import com.stefperf.impatient.chapter21.Chapter21

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

//execute only the chapters requested as command line arguments, or all chapters sequentially if no argument is given
object AllBook extends App {
  // force all objects to be initialized and available
  Seq(
    Chapter01, Chapter02, Chapter03, Chapter04, Chapter05, Chapter06, Chapter07,
    Chapter08, Chapter09, Chapter10, Chapter11, Chapter12, Chapter13, Chapter14,
//    Chapter15, Chapter16,
    Chapter17, Chapter18,
    // Chapter19, Chapter20, Chapter21
  ).foreach{ _.forceInit() }

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
