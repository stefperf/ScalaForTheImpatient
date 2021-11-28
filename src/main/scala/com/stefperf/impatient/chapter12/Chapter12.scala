package com.stefperf.impatient.chapter12

import com.stefperf.impatient._

object Chapter12 extends Chapter(12, "Higher-Order Functions", Level.L1) {
  override def exercises() {

    exercise(1) {
      def values(fun: (Int) => Int, low: Int, high: Int): Seq[(Int, Int)] = (low to high).map(i => (i, fun(i)))
      println("values(x => x * x, -5, 5):")
      println(values(x => x * x, -5, 5))
    }

    exercise(2) {
      println("Array(3, 7, 2, 1).reduceLeft(_ max _):")
      println(Array(3, 7, 2, 1).reduceLeft(_ max _))
    }

    exercise(3, 4) {
      def demoFactorial(title: String, func: Int => Int, low: Int = 0, high: Int = 9): Unit = {
        println(title)
        for (n <- low to high) {
          println(s"$n -> ${func(n)}")
        }
      }

      def factorial0(n: Int): Int = if (n == 0) 1 else (1 to n).reduceLeft(_ * _)
      demoFactorial("Factorial using reduceLeft:", factorial0)

      def factorial1(n: Int): Int = (1 to n).foldLeft(1)(_ * _)
      demoFactorial("Factorial using foldLeft:", factorial1)
    }

    exercise(5) {
      def largest(fun: (Int) => Int, inputs: Seq[Int]): Int = inputs.map(fun).reduceLeft(_ max _)
      println("largest(x => 10 * x - x * x, 1 to 10):")
      println(largest(x => 10 * x - x * x, 1 to 10))
    }

    exercise(6) {
      def largestAt(fun: (Int) => Int, inputs: Seq[Int]): Int =
        inputs.map(n => (n -> fun(n))).reduceLeft((a, b) => if (a._2 < b._2) b else a)._1
      println("largestAt(x => 10 * x - x * x, 1 to 10):")
      println(largestAt(x => 10 * x - x * x, 1 to 10))
    }

    exercise(7) {
      def adjustToPair(fun: (Int, Int) => Int): ((Int, Int)) => Int = t => fun(t._1, t._2)
      println("adjustToPair(_ * _)((6, 7)):")
      println(adjustToPair(_ * _)((6, 7)))
      val pairs = (1 to 10) zip (11 to 20)
      println(s"pairs = $pairs")
      println("pairs.map(adjustToPair(_ + _)(_)):")
      println(pairs.map(adjustToPair(_ + _)(_)))
    }

    exercise(8) {
      println("Seq(\"this\", \"is\", \"exercise\", \"eight\").corresponds(Seq(4, 2, 8,5))(_.length == _):")
      println(Seq("this", "is", "exercise", "eight").corresponds(Seq(4, 2, 8,5))(_.length == _))
    }

    exercise(9) {
      def myCorresponds[A, B](seqA: Seq[A], seqB: Seq[B], p: (A,B) => Boolean): Boolean =
        (seqA zip seqB).forall(t => p(t._1, t._2))
      println("myCorresponds(Seq(\"this\", \"is\", \"exercise\", \"eight\"), Seq(4, 2, 8,5), " +
        "(s: String, i: Int) => s.length == i):")
      println(
        myCorresponds(Seq("this", "is", "exercise", "eight"), Seq(4, 2, 8,5), (s: String, i: Int) => s.length == i))
      println("This time, it is necessary to fully declare the function argument, in order to satisfy the compiler.")
    }

    exercise(10) {
      def unless(condition: Boolean)(action: => Unit): Unit = if (!condition) action
      println("Here, for the first Boolean parameter, call-by-name or not makes no difference, " +
        "as it must be evaluated immediately anyway.")
      println("However, the second Unit parameter must be call-by-name, " +
        "so that it can be evaluated later only if needed.")
      println("Currying is not strictly needed, but it allows a nicer syntax when using this control abstraction.")
      println("For example:\n(1 to 5).foreach(i => unless(i == 3) {println(i)})")
      println(" -> ")
      (1 to 5).foreach(i => unless(i == 3) {println(i)})

    }
  }
}
