package com.stefperf.impatient.chapter01

import com.stefperf.impatient._

object Chapter01 extends Chapter(1, "The Basics", Level.A1) {
  override def exercises() {
    println("Most exercises of this chapter are skipped here as they are just theory questions or REPL exercises.")
    println()

    exercise(2) {
      import scala.math.sqrt
      val sqrtOf3 = sqrt(3)
      println(f"3 - (3 ^ 0.5) ^ 2 = ${3 - sqrtOf3 * sqrtOf3}")
    }

    exercise(6) {
      println(f"2 ^1024 = ${BigInt(2) pow 1024}")
    }

    exercise(7) {
      import scala.math.BigInt.probablePrime
      import scala.util.Random
      println(probablePrime(100, Random))
    }

    exercise(8) {
      import scala.math.log
      val nAlphanumerics = 36
      val log2_of_36 = log(nAlphanumerics) / log(2)

      def randomAlphanumeric(nChars: Int): String = {
        val nDigits = (nChars * log2_of_36).ceil.toInt
        scala.math.BigInt.probablePrime(nDigits, scala.util.Random).toString(nAlphanumerics).take(nChars)
      }

      println(f"10 random charachers in range [a-z0-9]: ${randomAlphanumeric(10)}")
    }

    exercise(9) {
      val myString = "Hello!"
      println(f"First and last characters of string '$myString': '${myString(0)}', '${myString.last}'")
    }
  }
}
