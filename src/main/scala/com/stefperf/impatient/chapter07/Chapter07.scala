package com.stefperf.impatient.chapter07

import com.stefperf.impatient._

object Chapter07 extends Chapter(7, "Packages and Imports") {
  override def exercises() {
    exercise(1) {
      import _root_.com.horstmann.impatient.{Chapter7Exercise1A, Chapter7Exercise1B}
      Chapter7Exercise1A.main(Array.empty)
      println()
      Chapter7Exercise1B.main(Array.empty)
      println()
    }

    exercise(2) {
      import com.stefperf.impatient.chapter07.horstmann._
      println("This block sees _root_.ScalaForTheImpatient.chapter07.horstmann instead of _root_.com.horstmann:")
      new Something().print()
    }

    exercise(3) {
      import random._
      setSeed(77)
      val n = 5
      println(f"$n random integers: " + (for (_ <- 1 to n) yield nextInt()).mkString(" "))
      println(f"$n random doubles in [-1, +1]: " + (for (_ <- 1 to n) yield f"${nextDouble()}%.3f ").mkString)
    }

    exercise(4) {
      println("(theory answer) Package objects are a Scala 2 workaround no longer needed in Scala 3.")
    }

    exercise(5) {
      println("(theory answer) private[com] makes a member visible in package com and all its contained packages.")
      println("That is not useful, since com is usually the top-level package containing all others.")
    }

    exercise(6, 7) {
      import java.util.{HashMap => JavaHashMap}
      import scala.collection.JavaConverters._
      import scala.collection.mutable.{HashMap => ScalaHashMap}
      def copyJavaHashMap2ScalaHashMap(jhm: JavaHashMap[String, Int], shm: ScalaHashMap[String, Int]): Unit = {
        for ((k, v) <- jhm.asScala) shm += k -> v
      }

      val jhm = new JavaHashMap[String, Int]()
      jhm.put("one", 1)
      jhm.put("two", 2)
      jhm.put("three", 3)
      val shm = ScalaHashMap[String, Int]("zero" -> 0)
      copyJavaHashMap2ScalaHashMap(jhm, shm)
      printmap(jhm.asScala, start = "Java HashMap = ")
      printmap(shm, start = "Scala HashMap = ")
    }

    exercise(8) {
      println("(theory answer) \"import java._; import javax._\" import lots of members.\n" +
        "That is a bad idea because it probably causes lots of name conflicts.")
    }

    exercise(9) {
      // import java.lang.System  // unneeded: java.lang is always implicitly imported

      val newline = sys.props("line.separator") // equivalent to System.getProperties.get("line.separator")

      // low-level implementation of reading from input stream
      def readLine(): String = {
        val input = new StringBuilder()
        var char: Char = System.in.read().toChar
        while (char.toString != newline) {
          input += char
          char = System.in.read().toChar
        }
        input.mkString
      }

      val username = System.getProperties.get("user.name")
      print(f"$username, type the password: > ")
      val input = readLine()
      if (input == "secret") println(f"Welcome, $username!")
      else {
        val errMsg = "wrong password"
        System.err.print(errMsg + "\n")
        println(f"Printed '$errMsg' on the standard error stream.")
      }
    }

    exercise(10) {
      val link = "https://docs.google.com/spreadsheets/d/1xyToGau13ZEQXTQh1sgbPUJNc3-NBYcrQxLigbwDU1E/edit?usp=sharing"
      val overriddenMembers = Array(
        "Cloneable", "Boolean", "Byte", "Double", "Float", "Long", "Process", "ProcessBuilder", "Short",
        "StringBuilder", "Exception", "deprecated",
      )
      println("The package scala overrides the following members of the package java.lang:")
      println(overriddenMembers.sortBy(_.toLowerCase()).mkString("\n"))
      println("Source: " + link)
    }
  }
}
