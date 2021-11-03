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

      print("indexes(\"Mississippi\") -> ")
      println(indexes("Mississippi"))
    }

    exercise(2) {
      import scala.collection.immutable.SortedMap
      def indexes(str: String): SortedMap[Char, Set[Int]] = {
        str.toUpperCase.zipWithIndex.foldLeft(SortedMap[Char, Set[Int]]()) { case (sortedMap, (char, index)) =>
          sortedMap + (char -> (sortedMap.getOrElse(char, Set[Int]()) + index))
        }
      }

      print("indexes(\"Mississippi\") -> ")
      println(indexes("Mississippi"))
    }

    exercise(3) {
      import scala.collection.mutable.ListBuffer
      def halveByCopying(lb: ListBuffer[Int]): List[Int] = lb.indices.filter(_ % 2 == 0).map(lb(_)).toList

      def halveByRemoving(lb: ListBuffer[Int]): Unit = lb.indices.reverse.filter(_ % 2 == 0).foreach{ lb.remove }

      val ubound = 50000
      val numbers = (1 to ubound).to[ListBuffer]
      timeIt(s"halveByCopying(<integers from 1 to $ubound>)"){ halveByCopying(numbers) }
      timeIt(s"halveByRemoving(<integers from 1 to $ubound>)"){ halveByRemoving(numbers) }
    }

    exercise(4) {
      def mapKeys[K, V](keys: Seq[K], keysToValues: Map[K, V]): Seq[V] = keys.flatMap(keysToValues.get)

      print("mapKeys(Array(\"Tom\", \"Fred\", \"Harry\"), Map(\"Tom\" -> 3, \"Dick\" -> 4, \"Harry\" -> 5)) -> ")
      printseqline(mapKeys(Array("Tom", "Fred", "Harry"), Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)))
    }

    exercise(5) {
      def myMkString(seq: Seq[String], sep: String = ""): String = seq.reduceLeft(_ + sep + _)

      print("myMkString((0 to 9).map(_.toString), \", \") -> ")
      println(myMkString((0 to 9).map(_.toString), ", "))
    }

    exercise(6) {
      val lst = (1 to 3).toList
      println(s"lst = $lst")
      println("Both expressions just rebuild the list:")
      print("(lst :\\ List[Int]())(_ :: _) -> ")
      println((lst :\ List[Int]())(_ :: _))
      print("(List[Int]() /: lst)(_ :+ _) -> ")
      println((List[Int]() /: lst)(_ :+ _))
      println("Both expressions can be modified to reverse the list:")
      print("[inefficient]   (lst :\\ List[Int]())((el, lstNew) => lstNew :+ el) -> ")
      println((lst :\ List[Int]())((el, lstNew) => lstNew :+ el))
      print("[ efficient ]   (List[Int]() /: lst)((lstNew, el) => el +: lstNew) -> ")
      println((List[Int]() /: lst)((lstNew, el) => el +: lstNew))

    }

    println("WORK IN PROGRESS")
  }

}
