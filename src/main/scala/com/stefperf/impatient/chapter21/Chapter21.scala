package com.stefperf.impatient.chapter21

import com.stefperf.impatient._
import com.stefperf.impatient.chapter11.Fraction

import java.awt.Point
import scala.annotation.tailrec

object Chapter21 extends Chapter(21, "Implicits", Level.L3) {

  // for various exercises
  def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]): T = if (order(a) < b) a else b

  // for exercise 2
  implicit class PercentageEnrichedInt(val from: Int) extends AnyVal {
    def +%(percentage: Double): Int = (from * (1 + percentage / 100)).toInt
  }

  // for exercise 3
  @tailrec private def tailRecFactorial(num: Int, previousProduct: Int = 1): Int = {
    if (num == 0) previousProduct
    else tailRecFactorial(num - 1, previousProduct * num)
  }

  implicit class FactorialEnrichedInt(val from: Int) extends AnyVal {
    def `!`: Int = tailRecFactorial(from)
  }

  // for exercise 5
  implicit class RichFraction(from: Fraction) extends Ordered[Fraction] {
    override def compare(that: Fraction): Int = from.n * that.d - that.n * from.d
  }

  // for exercise 6
  implicit class RichPoint1(from: Point) extends Ordered[Point] {
    override def compare(that: Point): Int = {
      val deltaX: Int = from.x - that.x
      if (deltaX == 0) from.y - that.y else deltaX
    }
  }

  // for exercise 7
  implicit class RichPoint2(from: Point) extends Ordered[Point] {
    override def compare(that: Point): Int = this.squareDistanceFromOrigin - that.squareDistanceFromOrigin
    def squareDistanceFromOrigin: Int = from.x * from.x + from.y * from.y
  }

  override def exercises() {

    exercise(1) {
      println("When the operator -> is found, the object to the left is implicitly converted to an instance of " +
        "Predef.ArrowAssoc[A], which defines the needed ->[B] method. That is possible because the automatically " +
        "imported module Predef defines an implicit conversion to ArrowAssoc.")
    }

    exercise(2) {
      println(s"120 +% 10 = ${120 +% 10}")
    }

    exercise(3) {
      println(s"5.! = ${5.!}")
    }

    exercise(4) {
      println("COMING SOON")
    }

    exercise(5) {
      println(s"smaller(Fraction(1, 7), Fraction(2, 9)) = ${smaller(Fraction(1, 7), Fraction(2, 9))}")
    }

    exercise(6) {
      println(s"smaller(new Point(1, 7), new Point(2, 3))(RichPoint1(_)) = " +
        s"${smaller(new Point(1, 7), new Point(2, 3))(RichPoint1(_))}")
    }

    exercise(7) {
      println(s"smaller(new Point(1, 7), new Point(2, 3))(RichPoint2(_)) = " +
        s"${smaller(new Point(1, 7), new Point(2, 3))(RichPoint2(_))}")
    }

    exercise(8) {
      println("COMING SOON")
    }

    println("COMING SOON")
  }
}
