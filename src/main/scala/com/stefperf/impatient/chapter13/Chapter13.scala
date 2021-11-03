package com.stefperf.impatient.chapter13

import com.stefperf.impatient._

object Chapter13 extends Chapter(13, "Collections") {
  override def exercises(): Unit = {
    exercise(1) {
      import scala.collection.mutable
      def indexes(str: String): mutable.SortedMap[Char, mutable.Set[Int]] = {
        val charToIndexes = mutable.SortedMap[Char, mutable.Set[Int]]()
        str.toUpperCase.zipWithIndex.foreach { case (char, index) =>
          if (!charToIndexes.contains(char)) charToIndexes(char) = mutable.Set(index)
          else charToIndexes(char) += index
        }
        charToIndexes
      }

      println("indexes(\"Mississippi\"):")
      println(indexes("Mississippi"))
    }

    exercise(2) {
      import scala.collection.immutable.SortedMap
      def indexes(str: String): SortedMap[Char, Set[Int]] = {
        str.toUpperCase.zipWithIndex.foldLeft(SortedMap[Char, Set[Int]]()) { case (sortedMap, (char, index)) =>
          sortedMap + (char -> (sortedMap.getOrElse(char, Set[Int]()) + index))
        }
      }

      println("indexes(\"Mississippi\"):")
      println(indexes("Mississippi"))
    }

    exercise(3) {
      import scala.collection.mutable.ListBuffer
      def halveByCopying(lb: ListBuffer[Int]): List[Int] = lb.indices.filter(_ % 2 == 0).map(lb(_)).toList

      def halveByRemoving(lb: ListBuffer[Int]): Unit = lb.indices.reverse.filter(_ % 2 == 0).foreach{ lb.remove(_) }

      val ubound = 50000
      val numbers = (1 to ubound).to[ListBuffer]
      timeIt(s"halveByCopying(<integers from 1 to $ubound>)"){ halveByCopying(numbers) }
      timeIt(s"halveByRemoving(<integers from 1 to $ubound>)"){ halveByRemoving(numbers) }
    }

    println("WORK IN PROGRESS")
  }
}
