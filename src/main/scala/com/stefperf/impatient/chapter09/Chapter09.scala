package com.stefperf.impatient.chapter09

import java.io.PrintWriter
import scala.io.Source

import com.stefperf.impatient._

object Chapter09 extends Chapter(9, "Files and Regular Expressions", Level.A1) {
  override def exercises() {

    exercise(1) {
      val dir = "./src/main/scala/com/stefperf/impatient/chapter09/"
      val inFileName = "Chapter09Exercise01"
      val outFileName = inFileName + "LineReversed"
      val ext = ".txt"
      val input = Source.fromFile(dir + inFileName + ext)
      println(s"-- File $inFileName$ext contains:")
      println(input.mkString)
      val output = new PrintWriter(dir + outFileName + ext)
      input.getLines.toArray.reverse.foreach {
        output.println
      }
      input.close()
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

      val dir = "./src/main/scala/com/stefperf/impatient/chapter09/"
      val inFileName = "Chapter09Exercise02"
      val outFileName = inFileName + "TabsVisualized"
      val ext = ".txt"
      val input = Source.fromFile(dir + inFileName + ext)
      val output = new PrintWriter(dir + outFileName + ext)
      println("(deliberately changing the specs for convenience' sake)")
      println(s"-- File $inFileName$ext contains:")
      println(input.mkString)
      for (line <- input.getLines) output.println(untab(line, filler = '.'))
      input.close()
      output.close()
      println(s"-- File $outFileName$ext contains:")
      println(Source.fromFile(dir + outFileName + ext).mkString)
    }

    exercise(3) {
      // putting all in one line here is actually not a good idea, as readability decreases
      println("-- Words with 12+ chars:\n" + (for (w <- """[A-Za-z]{12,}""".r.findAllIn(scala.io.Source.fromFile("./src/main/scala/com/stefperf/impatient/chapter09/Chapter09Exercise03.txt").mkString)) yield w).mkString("\n"))
    }

    exercise(4) {
      val text = scala.io.Source.fromFile(
        "./src/main/scala/com/stefperf/impatient/chapter09/Chapter09Exercise04.txt").mkString
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
        results("sum") = numbers.sum
        results("avg") = results("sum") / numbers.length
        results("min") = numbers.min
        results("max") = numbers.max
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
      val filename = "./src/main/scala/com/stefperf/impatient/chapter09/Chapter09Exercise05.txt"
      val output = new PrintWriter(filename)
      for (p <- powers; s = p.toString) output.println(s"${" " * (maxDigits - s.length)}$s          ${1D / p}")
      output.close()
      println("-- File contents:")
      println(Source.fromFile(filename).mkString)
    }

    exercise(6) {
      val pattern = """""|"\\""|"([^\\"]*|([^\\]{1}\\")*)*"|"[^"]*([^\\]\\\\){1}"""".r
      val filename = "./src/main/scala/com/stefperf/impatient/chapter09/Chapter09Exercise06.txt" + "" // empty str as test
      println("Quoted strings found in file:")
      for (quoted <- pattern.findAllIn(Source.fromFile(filename).mkString))
        println(s"$quoted")
    }

    exercise(7) {
      val floatPattern = """[-+]?(\d+(.\d*)?|.\d+)([Ee]{1}[-+]?\d+)?"""
      val text = Source.fromFile("./src/main/scala/com/stefperf/impatient/chapter09/Chapter09Exercise07.txt").mkString
      for (token <- text.split("[\\n\\s]") if token.nonEmpty && !token.matches(floatPattern))
        println(token)
    }

    exercise(8) {
      val url = "https://horstmann.com/scala"
      val text = Source.fromURL(url).mkString
      val imgPattern = """<img .*src="(.*?)".*>""".r("src")
      println(s"Image sources on page $url:")
      for (img <- imgPattern.findAllMatchIn(text)) println("- " + img.group("src"))
    }

    exercise(9) {
      import java.nio.file._
      val dirname = "./src/main/scala"
      val ext = ".scala" // for convenience' sake, searching .scala instead of .class
      val entries = Files.walk(Paths.get(dirname))
      try {
        val count = entries.filter(_.toString.takeRight(ext.length) == ext).toArray.length
        println(s"$count files with extension $ext were found in $dirname.")
      }
      catch {
        case _: Throwable =>
          println(s"Some problem happened wile counting files with extension $ext in $dirname.")
      }
      finally {
        entries.close()
      }
    }

    exercise(10) {
      import scala.collection.mutable.ArrayBuffer
      class Person(val name: String) extends Serializable {
        private val _friends = new ArrayBuffer[Person]

        def friends: ArrayBuffer[Person] = _friends.sortBy(_.name)

        def befriends(other: Person) {
          this._friends += other; other._friends += this
        }

        override def toString: String = name
      }
      val a = new Person("Alfred")
      val b = new Person("Boris")
      val c = new Person("Carlo")
      val persons = Array(a, b, c)
      b befriends c
      a befriends b
      println("- Before serialization:")
      for (p <- persons) println(s"${p.name}'s friends: ${p.friends.mkString(", ")}.")
      import java.io._
      val out = new ObjectOutputStream(new FileOutputStream("/tmp/test.obj"))
      out.writeObject(persons)
      out.close()
      val in = new ObjectInputStream(new FileInputStream("/tmp/test.obj"))
      val saved_persons = in.readObject().asInstanceOf[Array[Person]]
      in.close()
      println("- After serialization:")
      for (p <- saved_persons) println(s"${p.name}'s friends: ${p.friends.mkString(", ")}.")
    }
  }
}
