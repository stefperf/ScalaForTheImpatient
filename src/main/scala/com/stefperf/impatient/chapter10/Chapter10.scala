package com.stefperf.impatient.chapter10

import com.stefperf.impatient._

object Chapter10 extends Chapter(10, "Traits", {
  exercise(1) {
    trait RectangleLike {
      def getX: Double

      def getY: Double

      def getWidth: Double

      def getHeight: Double

      def setFrame(x: Double, y: Double, w: Double, h: Double): Unit

      def translate(dx: Double, dy: Double) = setFrame(getX + dx, getY + dy, getWidth, getHeight)

      def grow(x: Double, y: Double) = setFrame(getX - x, getY - y, getWidth + 2 * x, getHeight + 2 * y)

      def className = this.getClass.getName

      override def toString = s"RectangleLike[x=${getX},y=${getY},width=${getWidth},height=${getHeight}]"
    }

    println("- Behavior of java.awt.Rectangle ---")
    val rect = new java.awt.Rectangle(5, 10, 20, 30)
    println(s"rect = $rect")
    rect.translate(10, -10)
    println("...executing rect.translate(10, -10)...")
    println(s"rect = $rect")
    rect.grow(10, 20)
    println("...executing rect.grow(10, 20)...")
    println(s"rect = $rect")
    println()
    println("- Behavior of java.awt.geom.Ellipse2D ---")
    val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
    println(s"egg = $egg")
    egg.translate(10, -10)
    println("...executing egg.translate(10, -10)...")
    println(s"egg = $egg")
    egg.grow(10, 20)
    println("...executing egg.grow(10, 20)...")
    println(s"egg = $egg")
  }

  exercise(2) {
    import java.awt.Point
    class OrderedPoint(x: Int, y: Int) extends Point(x, y) with scala.math.Ordered[Point] {
      def compare(that: Point): Int = {
        if (this.x < that.x) -1
        else if (this.x > that.x) + 1
        else {
          if (this.y < that.y) -1
          else if (this.y > that.y) + 1
          else 0
        }
      }
      override def toString = s"($x, $y)"
    }
    val sortablePoints = Array(
      new OrderedPoint(3, 4),
      new OrderedPoint(3, 2),
      new OrderedPoint(1, 4),
      new OrderedPoint(3, 1),
      new OrderedPoint(3, 2),
    )
    println("OrderedPoints")
    printseqline(sortablePoints, start = "before sorting: ")
    printseqline(sortablePoints.sortWith(_ < _), start = "after sorting:  ")
  }

  exercise(3) {
    println("skipped because it is a pure theory exercise")
  }

  println("WORK IN PROGRESS")
})
