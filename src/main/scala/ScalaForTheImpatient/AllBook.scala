package ScalaForTheImpatient

import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

//execute only the chapters requested as command line arguments, or all chapters sequentially if no argument is given
object AllBook extends App {
  // force all objects to be initialized and available
  Chapter1.forceInit()
  Chapter2.forceInit()
  Chapter3.forceInit()
  Chapter4.forceInit()
  Chapter5.forceInit()
  Chapter6.forceInit()
  Chapter7.forceInit()
  Chapter8.forceInit()
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
