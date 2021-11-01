package com.stefperf.impatient.chapter04

import com.stefperf.impatient._

import scala.collection.{immutable, mutable}

object Chapter04 extends Chapter(4, "Maps and Tuples") {
  override def exercises() {

    exercise(1) {
      val fullPrices = Map[String, Double](
        ("product1", 100),
        ("product2", 200),
        ("product3", 49.99),
        ("product4", 19999.90)
      )
      val discountPerc = 0.1
      val discountedPrices = for ((k, v) <- fullPrices) yield (k, (v * (1 - discountPerc) * 100.0).round / 100.0)
      println("discountedPRices =")
      printmap(discountedPrices)
    }

    exercise(2) {
      val pathname = "./src/main/scala/com/stefperf/impatient/chapter04/Chapter04.scala"
      val in = new java.util.Scanner(new java.io.File(pathname))
      val wordRegex = raw"[a-zA-Z]+".r
      val wordCounts = mutable.Map[String, Int]()
      while (in.hasNext()) {
        val charAggregate = in.next()
        for (word <- wordRegex.findAllMatchIn(charAggregate).map(_.toString()))
          if (word != "") wordCounts(word) = wordCounts.getOrElse(word, 0) + 1
      }
      val n = 5
      println(f"Top $n most frequent 'words' in this file:")
      printmap(immutable.ListMap(wordCounts.toSeq.sortBy(-_._2): _*).take(n))
    }

    exercise(3) {
      val pathname = "./src/main/scala/com/stefperf/impatient/chapter04/Chapter04.scala"
      val in = new java.util.Scanner(new java.io.File(pathname))
      val wordRegex = raw"[a-zA-Z]+".r
      var wordCounts = immutable.Map[String, Int]()
      while (in.hasNext()) {
        val charAggregate = in.next()
        for (word <- wordRegex.findAllMatchIn(charAggregate).map(_.toString()))
          if (word != "") wordCounts = wordCounts + (word -> (wordCounts.getOrElse(word, 0) + 1))
      }
      val n = 5
      println(f"Top $n most frequent 'words' in this file:")
      printmap(immutable.ListMap(wordCounts.toSeq.sortBy(-_._2): _*).take(n))
    }

    exercise(4) {
      val pathname = "./src/main/scala/com/stefperf/impatient/chapter04/Chapter04.scala"
      val in = new java.util.Scanner(new java.io.File(pathname))
      val wordRegex = raw"[a-zA-Z]+".r
      val wordCounts = mutable.SortedMap[String, Int]()
      while (in.hasNext()) {
        val charAggregate = in.next()
        for (word <- wordRegex.findAllMatchIn(charAggregate).map(_.toString()))
          if (word != "") wordCounts(word) = wordCounts.getOrElse(word, 0) + 1
      }
      printmap(wordCounts)
    }

    exercise(5) {
      import scala.collection.JavaConverters._
      val pathname = "./src/main/scala/com/stefperf/impatient/chapter04/Chapter04.scala"
      val in = new java.util.Scanner(new java.io.File(pathname))
      val wordRegex = raw"[a-zA-Z]+".r
      val wordCounts: scala.collection.mutable.Map[String, Int] = new java.util.TreeMap[String, Int].asScala
      while (in.hasNext()) {
        val charAggregate = in.next()
        for (word <- wordRegex.findAllMatchIn(charAggregate).map(_.toString()))
          if (word != "") wordCounts(word) = wordCounts.getOrElse(word, 0) + 1
      }
      printmap(wordCounts)
    }

    exercise(6) {
      import java.util.Calendar
      val weekdayToCode = mutable.LinkedHashMap[String, Int]()
      weekdayToCode += ("Monday" -> Calendar.MONDAY)
      weekdayToCode += ("Tuesday" -> Calendar.TUESDAY)
      weekdayToCode += ("Wednesday" -> Calendar.WEDNESDAY)
      weekdayToCode += ("Thursday" -> Calendar.THURSDAY)
      weekdayToCode += ("Friday" -> Calendar.FRIDAY)
      weekdayToCode += ("Saturday" -> Calendar.SATURDAY)
      weekdayToCode += ("Sunday" -> Calendar.SUNDAY)
      printmap(weekdayToCode)
    }

    exercise(7) {
      import scala.collection.JavaConverters._
      val props: scala.collection.Map[String, String] = System.getProperties().asScala
      var maxNameLength = 0
      for (k <- props.keys) maxNameLength = maxNameLength max k.length
      for ((k, v) <- props.toSeq.sortBy(_._1))
        println(f"$k${" " * (maxNameLength - k.length)} | ${v.replace("\n", "\\n")}")
    }

    exercise(8) {
      // get smallest and largest value in the array
      def minmax(values: Array[Int]): (Int, Int) = {
        require(values.length > 0, "array values must contain at least 1 element")
        var arrmin, arrmax = values(0)
        for (v <- values.drop(1)) {
          arrmin = arrmin min v; arrmax = arrmax max v
        }
        (arrmin, arrmax)
      }

      val arr = Array(1, 4, 8, 2, 5, 0, 7, 4, 5)
      println(f"${seq2line(arr)} => ${minmax(arr)}")
    }

    exercise(9) {
      // counts of values less than v, equal to v, and greater than v
      def lteqgt(values: Array[Int], v: Int): (Int, Int, Int) = {
        var lt, eq, gt = 0
        for (va <- values) {
          if (va < v) lt += 1
          else if (va > v) gt += 1
          else eq += 1
        }
        (lt, eq, gt)
      }

      val arr = Array(1, 4, 8, 2, 5, 0, 7, 4, 5)
      println(f"${seq2line(arr)} => ${lteqgt(arr, 4)}")
    }

    exercise(10) {
      println("Use case: finding overlapping letters in the same position, with or without the same letter case.")

      def findOverlaps(word1: String, word2: String, caseSensitive: Boolean = false): Map[Int, Char] = {
        var w1 = word1
        var w2 = word2
        if (!caseSensitive) {
          w1 = word1.toUpperCase();
          w2 = word2.toUpperCase()
        }
        (for (((let1, let2), i) <- (w1 zip w2).zipWithIndex if let1 == let2) yield (i, let1)).toMap
      }

      def printOverlaps(word1: String, word2: String, caseSensitive: Boolean = false): Unit = {
        val overlaps = findOverlaps(word1, word2, caseSensitive)
        val cs = if (caseSensitive) ", same-case" else ""
        if (overlaps.isEmpty) println(f"Words '$word1' and '$word2' have no same-position$cs letter overlap.")
        else {
          if (overlaps.size == 1) println(f"Words '$word1' and '$word2' have this same-position$cs letter overlap:")
          else println(f"Words '$word1' and '$word2' have these same-position$cs letter overlaps:")
          overlaps.foreach { case (pos, letter) => println(f"'$letter' as letter nr. ${pos + 1}") }
        }
      }

      println()
      printOverlaps("Hello", "Stefano")
      println()
      printOverlaps("Hello", "World")
      println()
      printOverlaps("Hello", "hollow")
      println()
      printOverlaps("Hello", "hollow", caseSensitive = true)
    }
  }
}
