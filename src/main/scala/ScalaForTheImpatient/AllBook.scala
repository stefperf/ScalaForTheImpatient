package ScalaForTheImpatient

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

import ScalaForTheImpatient.chapter02.Chapter02
import ScalaForTheImpatient.chapter03.Chapter03
import ScalaForTheImpatient.chapter04.Chapter04
import ScalaForTheImpatient.chapter05.Chapter05
import ScalaForTheImpatient.chapter06.Chapter06
import ScalaForTheImpatient.chapter07.Chapter07
import ScalaForTheImpatient.chapter08.Chapter08
import ScalaForTheImpatient.chapter09.Chapter09

//execute only the chapters requested as command line arguments, or all chapters sequentially if no argument is given
object AllBook extends App {
  // force all objects to be initialized and available
  ScalaForTheImpatient.chapter01.Chapter01.forceInit()
  Chapter02.forceInit()
  Chapter03.forceInit()
  Chapter04.forceInit()
  Chapter05.forceInit()
  Chapter06.forceInit()
  Chapter07.forceInit()
  Chapter08.forceInit()
  Chapter09.forceInit()
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
