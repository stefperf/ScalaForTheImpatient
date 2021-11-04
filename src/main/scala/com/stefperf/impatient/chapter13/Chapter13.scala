package com.stefperf.impatient.chapter13

import com.stefperf.impatient._

import scala.collection.immutable.SortedMap

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

    exercise(7) {
      val prices = Seq(1.1, 1.2, 1.3)
      val quantities = Seq(11, 12, 13)
      println(s"prices = $prices, quantities = $quantities")
      println(s"(prices zip quantities) map { t => t._1 * t._2 } = ${(prices zip quantities) map { t => t._1 * t._2 }}")
      println(s"(prices zip quantities) map { Function.tupled(_ * _) } = ${(prices zip quantities) map { Function.tupled(_ * _) }}")
    }

    exercise(8) {
      def to2DArr(arr: Array[Double], nCols: Int, fillerEl: Double = Double.NaN): Array[Array[Double]] = {
        val nFillersNeeded = if (arr.length % nCols == 0) 0 else (arr.length / nCols + 1) * nCols - arr.length
        (arr ++ (1 to nFillersNeeded).map(_ => fillerEl)).grouped(nCols).map(_.toArray).toArray
      }
      println("to2DArr(Array(1, 2, 3, 4, 5, 6), 3) =\n" + arr2dToString(to2DArr(Array(1, 2, 3, 4, 5, 6), 3)))
      println()
      println("to2DArr(Array(1, 2, 3, 4, 5), 3) =\n" + arr2dToString(to2DArr(Array(1, 2, 3, 4, 5), 3)))
    }

    exercise(9) {
      println("flatMap is used for all but the last generator inside the `for` parentheses," +
        " so as to retain only one \"level\". Example with 3 generators:")
      println(s"for (i <- 1 to 3; j <- 1 to i; k <- 1 to j) yield i * j * k" +
        s":\n ${for (i <- 1 to 3; j <- 1 to i; k <- 1 to j) yield i * j * k}")
      println()
      println(s"(1 to 3).flatMap(i => (1 to i).flatMap(j => (1 to j).map(k => i * j * k)))" +
        s":\n ${(1 to 3).flatMap(i => (1 to i).flatMap(j => (1 to j).map(k => i * j * k)))}")
    }

    exercise(10) {
      val continentalTimeZonePattern = raw"(\w+)/\w+".r
      val timeZones = java.util.TimeZone.getAvailableIDs
      println(s"Time zones:\n${timeZones.sorted.mkString("\n")}")
      val (continent, maxNrTimeZones) = timeZones.flatMap {
        case continentalTimeZonePattern(continent) if continent.toUpperCase != "ETC" => Some(continent)
        case _ => None
      }.groupBy(continent => continent).map {
        case (continent, seq) => (continent, seq.length)
      }.maxBy(Function.tupled((_, b) => b))
      println(s"The continent with most time zones is $continent, having $maxNrTimeZones time zones.")
    }

    exercise(11) {
      println("The given code involves unchecked concurrent read/write of shared memory. Results would be random.")

      val pathname = "./src/main/scala/com/stefperf/impatient/chapter13/Chapter13.scala"
      val text = scala.io.Source.fromFile(pathname).mkString
      val charCounts = SortedMap[Char, Int]() ++ text.foldLeft(SortedMap[Char, Int]()){
        (m, c) => m.+(c -> (m.getOrElse(c, 0) + 1))
      }
      printmap(charCounts, start = "Character frequencies in this file, counted without parallelism:\n")
      val charCountsPar = SortedMap[Char, Int]() ++ text.par.aggregate({
        println("<thread>")
        Map[Char, Int]()
      })(
        (m, c) => m.+(c -> (m.getOrElse(c, 0) + 1)), { (map0, map1) =>
          (map0.toSeq ++ map1.toSeq).groupBy(tup => tup._1).map{ case (ch, tups) => (ch, tups.map(_._2).sum)}
      })
      printmap(charCountsPar, start = "Character frequencies in this file, counted with parallelism:\n")
    }

  }

}
