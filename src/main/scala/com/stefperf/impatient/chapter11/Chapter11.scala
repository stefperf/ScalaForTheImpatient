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
    // class Fraction implemented in Fraction.scala with some improvements over exercise specs
    def parenNeg(f: Fraction) = if (f.sign < 0) s"($f)" else f.toString
    val f1 = new Fraction(2, 3)
    for (
      f2 <- Seq(new Fraction(-3, -4), new Fraction(3, -4));
      (f1_operator, symbol) <- Seq(f1.+ _, f1.- _, f1.* _, f1./ _) zip Seq('+', '-', '*', '/')
    )
      println(s"$f1 $symbol ${parenNeg(f2)} = ${f1_operator(f2)}")
  }

  exercise(4) {
    // class Money implemented in Money.scala
    println(s"Money(1, 75) + Money(0, 50) == Money(2, 25): ${Money(1, 75) + Money(0, 50) == Money(2, 25)}")
    println("It is at least questionable to implement operator *, as money cannot be multiplied by money.")
    println("It is at least questionable to implement operator /, as money divided by money yields a pure number.")
  }

  exercise(5) {
    println("not implemented yet")
  }
  exercise(6) {
    println("not implemented yet")
  }
  exercise(7) {
    println("not implemented yet")
  }
  exercise(8) {
    println("not implemented yet")
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
