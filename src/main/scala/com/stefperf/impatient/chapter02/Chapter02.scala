package com.stefperf.impatient.chapter02

import com.stefperf.impatient._

object Chapter02 extends Chapter(2, "Control Structures and Functions", Level.A1) {

  override def exercises() {
    exercise(1) {
      def signum(num: Double): Int = {
        if (num < 0) -1
        else if (num == 0) 0
        else +1
      }

      println(signum(0.0))
    }
    exercise(4, 5) {
      def countdown(n: Int = 10): Unit = {
        for (i <- n to 0 by -1) println(i)
      }

      countdown()
    }

    exercise(6) {
      def unicodeProd(str: String): Long = {
        var prod = 1L
        for (ch <- str) prod *= ch.toLong
        prod
      }

      println(unicodeProd("Hello"))
    }

    exercise(7, 8) {
      def product(str: String): Long =
        str.foldLeft(1L)(_ * _)

      println(product("Hello"))
    }

    exercise(9) {
      def productRec(str: String): Long =
        if (str == "") 1L else str(0) * productRec(str drop 1)

      println(productRec("Hello"))
    }

    exercise(10) {
      import scala.math.{E, pow}
      def myPow(x: Double, n: Int): Double = {
        if (n == 0) 1d
        else if (n < 0) 1d / myPow(x, -n)
        else if (n % 2 == 0) pow(myPow(x, n / 2), 2)
        else x * myPow(x, n - 1)
      }

      println(myPow(E, 1), myPow(2, -2), myPow(10, 3))
    }

    exercise(11) {
      Chapter02Exercise11.main(Array.empty)
    }
  }

}
