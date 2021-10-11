package ScalaForTheImpatient.chapter09

import java.io.PrintWriter
import scala.io.Source

import ScalaForTheImpatient._

object Chapter09 extends Chapter(9, "Files and Regular Expressions", {
  exercise(1) {
    val dir = "./src/main/scala/ScalaForTheImpatient/Chapter09/"
    val inFileName = "Chapter09Exercise01"
    val outFileName = inFileName + "LineReversed"
    val ext = ".txt"
    val input = Source.fromFile(dir + inFileName + ext)
    println(s"-- File $inFileName$ext contains:")
    println(Source.fromFile(dir + inFileName + ext).mkString)
    val output = new PrintWriter(dir + outFileName + ext)
    input.getLines.toArray.reverse.foreach {
      output.println(_)
    }
    output.close()
    println(s"-- File $outFileName$ext contains:")
    println(Source.fromFile(dir + outFileName + ext).mkString)
  }

  exercise(2) {
    def untab(line: String, tabSize: Int = 4, filler: Char = ' '): String = {
      def nextTabLength(col: Int) = tabSize - col % tabSize

      val sb = new StringBuilder()
      for (c <- line) {
        if (c == '\t') sb ++= filler.toString * nextTabLength(sb.length)
        else sb += c
      }
      sb.toString
    }

    val dir = "./src/main/scala/ScalaForTheImpatient/Chapter09/"
    val inFileName = "Chapter09Exercise02"
    val outFileName = inFileName + "TabsVisualized"
    val ext = ".txt"
    println("(deliberately changing the specs for convenience' sake)")
    println(s"-- File $inFileName$ext contains:")
    println(Source.fromFile(dir + inFileName + ext).mkString)
    val input = Source.fromFile(dir + inFileName + ext)
    val output = new PrintWriter(dir + outFileName + ext)
    for (line <- input.getLines) output.println(untab(line, filler = '.'))
    output.close()
    println(s"-- File $outFileName$ext contains:")
    println(Source.fromFile(dir + outFileName + ext).mkString)
  }

  exercise(3) {
    // putting all in one line here is actually not a good idea, as readability decreases
    println("-- Words with 12+ chars:\n" + (for (w <- """[A-Za-z]{12,}""".r.findAllIn(scala.io.Source.fromFile("./src/main/scala/ScalaForTheImpatient/Chapter09/Chapter09Exercise03.txt").mkString)) yield w).mkString("\n"))
  }

  exercise(4) {
    val text = scala.io.Source.fromFile(
      "./src/main/scala/ScalaForTheImpatient/Chapter09/Chapter09Exercise04.txt").mkString
    val numbers = new scala.collection.mutable.ArrayBuffer[Double]()
    println("-- Numbers found in file:")
    for (w0 <- text.split("\\s") if w0.nonEmpty) {
      try {
        val d = w0.toDouble
        val w1 = d.toString
        numbers += d
        println(s"#${numbers.length}: " + (if (w0 == w1) w0 else s"$w0 = $w1"))
      } catch {
        case _: Throwable => // deliberately doing nothing
      }
    }
    if (numbers.nonEmpty) {
      val results = new scala.collection.mutable.ListMap[String, Double]()
      results("sum") = numbers.reduce(_ + _)
      results("avg") = results("sum") / numbers.length
      results("min") = numbers.reduce(_ min _)
      results("max") = numbers.reduce(_ max _)
      println("-- Stats:")
      for ((k, v) <- results) println(f"$k = $v%.3f")
    }
  }

  exercise(5) {
    val base = 2
    val maxExp = 20
    val powers = new Array[Long](maxExp + 1)
    var power = 1L
    for (n <- 0 to maxExp) {
      powers(n) = power
      power *= base
    }
    val maxDigits = powers.last.toString.length
    val filename = "./src/main/scala/ScalaForTheImpatient/Chapter09/Chapter09Exercise05.txt"
    val output = new PrintWriter(filename)
    for (p <- powers; s = p.toString) output.println(s"${" " * (maxDigits - s.length)}$s          ${1D / p}")
    output.close()
    println("-- File contents:")
    println(Source.fromFile(filename).mkString)
  }

  exercise(6) {
    val pattern = """""|"\\""|"([^\\"]*|([^\\]{1}\\")*)*"|"[^"]*([^\\]\\\\){1}"""".r
    val filename = "./src/main/scala/ScalaForTheImpatient/Chapter09/Chapter09Exercise06.txt" + ""  // empty str as test
    println("Quoted strings found in file:")
    for (quoted <- pattern.findAllIn(Source.fromFile(filename).mkString))
      println(s"$quoted")
  }

  println("WORK IN PROGRESS...")
})

