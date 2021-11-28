package com.stefperf.impatient.chapter06

import com.stefperf.impatient._

object Chapter06 extends Chapter(6, "Objects", Level.A1) {

  override def exercises() {
    exercise(1) {
      object Conversions {
        def inchesToCentimeters(in: Double): Double = 2.54 * in

        def gallonsToLiters(in: Double): Double = 3.78541 * in

        def milesToKilometers(in: Double): Double = 1.60934 * in
      }
      import Conversions._
      val conversions = Array[(Double, String, String, Double => Double)](
        (100, "mile", "kilometer", milesToKilometers),
        (12.5, "gallon", "liter", gallonsToLiters),
        (42, "inch", "centimeter", inchesToCentimeters),
      )
      for ((measure0, unit0, unit1, conversion01) <- conversions; measure1 = conversion01(measure0))
        println(f"$measure0 $unit0 = $measure1 $unit1")
    }

    exercise(2) {
      class UnitConversion(val ratio: Double) {
        def convert(input: Double): Double = ratio * input
      }
      object InchesToCentimeters extends UnitConversion(2.54)
      object GallonsToLiters extends UnitConversion(3.78541)
      object MilesToKilometers extends UnitConversion(1.60934)
      val conversions = Array[(Double, String, String, UnitConversion)](
        (100, "mile", "kilometer", MilesToKilometers),
        (12.5, "gallon", "liter", GallonsToLiters),
        (42, "inch", "centimeter", InchesToCentimeters),
      )
      for ((measure0, unit0, unit1, converter01) <- conversions; measure1 = converter01.convert(measure0))
        println(f"$measure0 $unit0 = $measure1 $unit1")
    }

    exercise(3) {
      // not a good idea because Point is lightweight, mutable and with no static methods: no use for a singleton pattern
      object Origin extends java.awt.Point {
        def set(x: Double, y: Double): Unit = super.setLocation(x, y)
      }
      Origin.setLocation(3, 4)
      println(f"Origin = (x=${Origin.x}, y=${Origin.y}")
    }

    exercise(4) {
      class Point(val x: Double, val y: Double) {
        override def toString: String = f"(x=$x, y=$y)"
      }
      object Point {
        def apply(x: Double, y: Double): Point = new Point(x, y)
      }
      println(f"new Point(3, 4) = ${new Point(3, 4)}")
      println(f"Point(3, 4) = ${Point(3, 4)}")
    }

    exercise(5) {
      object myApp extends App {
        println(args.reverse.mkString(" "))
      }
      myApp.main(Array("arguments", "to", "reverse"))
    }

    exercise(6) {
      object CardSuit extends Enumeration {
        type CardSuit = Value
        // Scala code supports both Unicode code points and directly used Unicode characters
        val Hearts = Value("\u2665")
        val Diamonds = Value("♦")
        val Clubs = Value("\u2663")
        val Spades = Value("♠")
      }
      for (cardSuit <- CardSuit.values; codepoint = BigInt(cardSuit.toString()(0).toInt).toString(16)) {
        println(f"card suit = $cardSuit, Unicode codepoint = \\u$codepoint")
      }
    }

    exercise(7) {
      object CardSuit extends Enumeration {
        type CardSuit = Value
        // Scala code supports both Unicode code points and directly used Unicode characters
        val Hearts = Value("\u2665")
        val Diamonds = Value("♦")
        val Clubs = Value("\u2663")
        val Spades = Value("♠")
      }
      import CardSuit._
      def isRed(cs: CardSuit): Boolean = (cs == Hearts || cs == Diamonds)

      for (cardSuit <- CardSuit.values; codepoint = BigInt(cardSuit.toString()(0).toInt).toString(16)) {
        println(f"card suit $cardSuit is " + (if (isRed(cardSuit)) "red" else "black"))
      }
    }

    exercise(8) {
      object RGBColorVertex extends Enumeration {
        type RGBColorVertex = Value
        val Red = Value(0xff0000, "red")
        val Green = Value(0x00ff00, "green")
        val Blue = Value(0x0000ff, "blue")
        val White = Value(0xffffff, "white")
        val Black = Value(0x000000, "black")
        val Cyan = Value(0x00ffff, "cyan")
        val Magenta = Value(0xff00ff, "magenta")
        val Yellow = Value(0xffff00, "yellow")
      }
      for (cv <- RGBColorVertex.values) println(f"$cv, 0x${BigInt(cv.id).toString(16)}")
    }
  }
}
