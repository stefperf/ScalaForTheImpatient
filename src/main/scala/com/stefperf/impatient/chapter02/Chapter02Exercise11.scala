package com.stefperf.impatient.chapter02

// this exercise is implemented here due to restrictions on where implicits and value classes may be declared
object Chapter02Exercise11 extends App {

  import java.time.LocalDate

  implicit class DateInterpolator(val sc: StringContext) extends AnyVal {
    def date(args: Any*): LocalDate = {
      if (args.length != 3) throw new IllegalArgumentException("argument number is wrong, they must be 3")
      if (sc.parts != List("", "-", "-", "")) throw new IllegalArgumentException("format must be \"year-month-day\"")
      val paras = new Array[Int](3)
      for ((arg, i) <- args.zipWithIndex) {
        arg match {
          case _: Int => paras(i) = arg.toString.toInt
          case _ => throw new IllegalArgumentException("all arguments must be Int")
        }
      }
      LocalDate.of(paras(0), paras(1), paras(2))
    }
  }

  // val myDate = date"$2021-$9-$25"
  val year = 2021
  val month = 9
  val day = 25
  val myDate = date"$year-$month-$day"
  println(myDate)
  println()

}
