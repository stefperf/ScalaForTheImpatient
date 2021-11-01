package com.stefperf

import scala.collection.mutable.ArrayBuffer

package object impatient {

  val LineLength = 120

  def fillLine(line: String, filler: String, lineLength: Int = LineLength): String =
    line + filler.head.toString * (LineLength - line.length)

  def seq2line[T](seq: Iterable[T]): String = seq.iterator.map(_.toString).mkString(start = "(", sep = ", ", end = ")")

  def printseqline[T](seq: Iterable[T], start: String = "", end: String = "\n"): Unit =
    print(start + seq2line(seq) + end)

  def map2str[K, V](map: scala.collection.Map[K, V], lineStart: String = "  ", sep: String = ",\n"): String =
    (for ((k, v) <- map) yield f"$lineStart($k => $v)").mkString(sep)

  def printmap[K, V](map: scala.collection.Map[K, V], lineStart: String = "  ", sep: String = ",\n",
                     start: String = "", end: String = "\n)\n"): Unit =
    print(start + "(\n" + map2str(map, lineStart, sep) + end)


  abstract class Chapter(val chapterNumber: Int, title: String) {
    Chapter.chaptersByNumber(chapterNumber) = this  // register this chapter
    def header: String = Chapter.header(chapterNumber, title)

    def main(args: Array[String]): Unit = Chapter.frameBody(chapterNumber, title, exercises)

    def forceInit(): Unit = {}  // just trigger construction execution

    def exercise(exerciseNumbers: Int*)(exerciseBody: => Unit): Unit = {
      if (exerciseNumbers.isEmpty)
        throw new IllegalArgumentException("At least 1 exercise number must be input.")
      val nums = exerciseNumbers.map(_.toString).mkString(", ")
      val singularOrPlural = if (exerciseNumbers.length > 1) Array("s", " together") else Array("", "")
      val FillCh = "-"
      println(fillLine(f"${FillCh * 3} Chapter $chapterNumber " +
        f"Exercise${singularOrPlural(0)} $nums${singularOrPlural(1)} ", FillCh))
      exerciseBody
      println()
    }

    def exercises(): Unit
  }

  object Chapter {
    private val FillCh = "="
    private val chaptersByNumber = scala.collection.mutable.LinkedHashMap[Int, Chapter]()

    protected def header(number: Int, title: String) = f"Chapter $number. $title"

    protected def frameBody(number: Int, title: String, body: () => Unit): Unit = {
      println(FillCh * LineLength)
      println(fillLine(f"${FillCh * 3} ${header(number, title)} ", FillCh))
      println()
      body()
      println()
      println()
    }

    def execute(numbers: Int*): Unit = {
      val numsRequested = if (numbers.nonEmpty) numbers else chaptersByNumber.keys
      val headersExecuted = ArrayBuffer[String]()
      for (num <- numsRequested) {
        if (chaptersByNumber.contains(num)) {
          val chapter = chaptersByNumber(num)
          chapter.main(Array.empty)
          headersExecuted += chapter.header
        }
        else frameBody(num, "", () => {println("not isImplemented")})
      }
      if (headersExecuted.nonEmpty) {
        println(FillCh * LineLength)
        println(fillLine(FillCh * 3 + " Chapters executed: ", FillCh))
        headersExecuted foreach {println(_)}
      }
    }

    def isImplemented(number: Int): Boolean = chaptersByNumber.contains(number)
  }

}
