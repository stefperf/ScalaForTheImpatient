package com.stefperf.impatient.chapter10

import com.stefperf.impatient._

import java.beans.{PropertyChangeEvent, PropertyChangeListener}

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

      override def toString = s"RectangleLike[x=$getX,y=$getY,width=$getWidth,height=$getHeight]"
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

  exercise(4){
    trait Logger {
      def log(msg: String): Unit
    }
    trait CryptoLogger extends Logger {
      val cryptoKey: Int
      private lazy val encode = (text: String) => {
        text.map((ch: Char) => {(ch.toInt + cryptoKey).toChar})
      }
      abstract override def log(text: String) = super.log(encode(text))
    }
    trait ConsoleLogger extends Logger {
      def log(msg: String): Unit = {
        println(msg)
      }
    }
    val testText = "this is the test text"
    for (key <- Array(+3, -3)) {
      println(f"Encoding the test text '$testText' with Caesar cipher = $key:")
      new ConsoleLogger with CryptoLogger {val cryptoKey: Int = key}.log(testText)
    }
  }

  exercise(5) {
    import java.awt.Point
    import java.beans.{PropertyChangeEvent => PCE, PropertyChangeListener => PCL, PropertyChangeSupport => PCS}
    trait MyPropertyChangeSupport {
      val pcs = new PCS(this)
      def addPropertyChangeListener(listener: PCL) = pcs.addPropertyChangeListener(listener)
      def removePropertyChangeListener(listener: PCL) = pcs.removePropertyChangeListener(listener)
    }
    trait ListenedLocationXY extends Point with MyPropertyChangeSupport {
      val name: String
      override def move(x: Int, y: Int): Unit = {
        val oldValue = (this.x, this.y)
        val newValue = (x, y)
        super.move(x, y)
        pcs.firePropertyChange(name, oldValue, newValue)
      }
      override def translate(dx: Int, dy: Int): Unit = move(x + dx, y + dy)
    }
    val point = new Point with ListenedLocationXY {val name = "myPoint"}
    point.addPropertyChangeListener((evt: PCE) =>
      println(s"property '${evt.getPropertyName}' changed from ${evt.getOldValue} to ${evt.getNewValue}"))
    point.setLocation(1, 2)
    point.move(3, 4)
    point.setLocation(new Point(5, 6))
    point.translate(2, 2)
  }

  exercise(6) {
    println("skipped because it is a pure theory exercise")
  }

  exercise(7) {
    println("WORK IN PROGRESS")  // TODO
  }

  exercise(8) {
    println("WORK IN PROGRESS")  // TODO
  }

  exercise(9) {
    println("-- (Simplified implementation, for pure demo purposes) --")
    val filepath = "./src/main/scala/com/stefperf/impatient/Chapter10/"
    val filename = "Chapter10Exercise09.txt"
    println("-- Reading file $filename one character at a time... --")
    import java.io._
    val EOF = -1
    trait MyBufferedInputStream extends java.io.InputStream {
      val bufSize: Int
      protected lazy val buf = new Array[Byte](bufSize)
      protected var count, pos = 0
      protected def nextByte(): Int = {
        val next = buf(pos)
        pos += 1
        next
      }
      override def read(): Int = {
        if (pos < count) nextByte()
        else {
          println(s"The $count-bytes buffer '${buf.take(count).map(_.toChar).mkString}' " +
            s"has been exhausted; trying to read more bytes...")  // for demo
          pos = 0
          val nrBytesRead = read(buf)
          if (nrBytesRead > 0) {
            count = nrBytesRead
            nextByte()
          }
          else {
            println("The input stream has finished.")
            count = 0
            EOF
          }
        }
      }
    }
    val inFile = new File(filepath + filename)
    val in = new java.io.FileInputStream(inFile) with MyBufferedInputStream {val bufSize = 10}
    var hasNext = true
    while (hasNext) {
      val next = in.read()
      if (next == EOF) hasNext = false
      else println(next.toChar)
    }
  }

  println("WORK IN PROGRESS")
})
