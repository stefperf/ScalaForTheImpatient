package com.stefperf.impatient.chapter11

import com.stefperf.impatient._

import scala.math.abs

object Chapter11 extends Chapter(11, "Operators", {
  exercise(1) {
    println(s"3 + 4 -> 5 == (3 + 4) -> 5 == ${3 + 4 -> 5}, " +
      s"from left to right, because both operators have the same precedence.")
    println(s"3 + 4 -> 5 is legal only as 3 -> (4 + 5) == ${3 -> (4 + 5)}.")
  }

  exercise(2) {
    println("Operators ** or ^ for power would have an unintuitively too low precedence, based on Scala's rules.")
  }

  exercise(3) {
    Chapter11Exercise03.main(Array.empty)
  }

  exercise(4) {
    Chapter11Exercise04.main(Array.empty)
 }

  exercise(5) {
    Chapter11Exercise05.main(Array.empty)
  }

  exercise(6) {
    Chapter11Exercise06.main(Array.empty)
  }

  exercise(7) {
    Chapter11Exercise07.main(Array.empty)
  }

  exercise(8) {
    Chapter11Exercise08.main(Array.empty)
  }
  exercise(9) {
    println("not implemented yet")
  }
  exercise(10) {
    println("not implemented yet")
  }
  exercise(11) {
    println("not implemented yet")
  }
  exercise(12) {
    println("not implemented yet")
  }
  exercise(13) {
    println("not implemented yet")
  }
})
