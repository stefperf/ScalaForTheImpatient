package com.stefperf.impatient.chapter10

import com.stefperf.impatient._

object Chapter10 extends Chapter(10, "Traits") {
  override def exercises() {
    exercise(1) {
      trait RectangleLike {
        def getX: Double

        def getY: Double

        def getWidth: Double

        def getHeight: Double

        def setFrame(x: Double, y: Double, w: Double, h: Double): Unit

        def translate(dx: Double, dy: Double) = setFrame(getX + dx, getY + dy, getWidth, getHeight)

        def grow(x: Double, y: Double) = setFrame(getX - x, getY - y, getWidth + 2 * x, getHeight + 2 * y)

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
          else if (this.x > that.x) +1
          else {
            if (this.y < that.y) -1
            else if (this.y > that.y) +1
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

    exercise(4) {
      trait Logger {
        def log(msg: String): Unit
      }
      trait CryptoLogger extends Logger {
        val cryptoKey: Int
        private lazy val encode = (text: String) => {
          text.map((ch: Char) => {
            (ch.toInt + cryptoKey).toChar
          })
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
        new ConsoleLogger with CryptoLogger {
          val cryptoKey: Int = key
        }.log(testText)
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

        override def setLocation(x: Int, y: Int): Unit = move(x, y)

        override def setLocation(x: Double, y: Double): Unit = move(math.round(x).toInt, math.round(y).toInt)

        override def setLocation(p: Point): Unit = move(p.x, p.y)

        override def translate(dx: Int, dy: Int): Unit = move(x + dx, y + dy)
      }
      val point = new Point(0, 1) with ListenedLocationXY {
        val name = "myPoint"
      }
      point.addPropertyChangeListener((evt: PCE) =>
        println(s"property '${evt.getPropertyName}' changed from ${evt.getOldValue} to ${evt.getNewValue}"))
      point.move(2, 3)
      point.setLocation(4, 5)
      point.setLocation(6.3, 6.9)
      point.setLocation(new Point(8, 9))
      point.translate(2, 2)
    }

    exercise(6) {
      println("skipped because it is a pure theory exercise")
    }

    exercise(7) {
      val savingsAccount = new SavingsAccount(1)
      savingsAccount.deposit(100)
      savingsAccount.withdraw(200)
      printf("(to see the point of this exercise, it is necessary to write and compile the code incrementally)")
    }

    exercise(8) {
      trait LayeredAnagrammer {
        val originalWord: String
        val minLength = 3
        lazy val minChanges: Int = originalWord.length / 2
        lazy val maxChanges: Int = originalWord.length

        protected def nextInRange(minInt: Int, maxInt: Int): Int = minInt + util.Random.nextInt(maxInt + 1 - minInt)

        protected def getNChangesRandomly: Int = nextInRange(minChanges, maxChanges)

        def permute: String = {
          require(originalWord.length >= minLength, s"originalWord must havve at least $minLength characters")
          originalWord
        }
      }
      trait ReversingAnagrammer extends LayeredAnagrammer {
        override def permute: String = super.permute.reverse
      }
      trait CuttingAnagrammer extends LayeredAnagrammer {
        def cutPosition: Int

        def cut(word: String): String = {
          require(1 <= cutPosition && cutPosition <= originalWord.length - 1, "cutPosition out of bounds")
          word.drop(cutPosition) + word.take(cutPosition)
        }

        override def permute: String = cut(super.permute)
      }
      trait RandomCuttingAnagrammer extends CuttingAnagrammer {
        def cutPosition: Int = nextInRange(1, originalWord.length - 1)
      }
      trait HalfCuttingAnagrammer extends CuttingAnagrammer {
        val cutPosition: Int = originalWord.length / 2
      }
      val word = "ANAGRAM"
      val anagram1 = (new {
        val originalWord = word
      } with ReversingAnagrammer with HalfCuttingAnagrammer).permute
      val anagram2 = (new {
        val originalWord = word
      } with HalfCuttingAnagrammer with ReversingAnagrammer).permute
      println(s"ReversingAnagrammer with HalfCuttingAnagrammer: $anagram1")
      println(s"HalfCuttingAnagrammer with ReversingAnagrammer: $anagram2")

    }

    exercise(9, 10) {
      // logging traits
      trait Logger {
        def log(msg: String)
      }
      trait ActivatableLogger extends Logger {
        var doLog: Boolean

        abstract override def log(msg: String) {
          if (doLog) super.log(msg)
        }
      }
      trait ConsoleLogger extends Logger {
        def log(msg: String) {
          println(msg)
        }
      }
      // buffered input stream
      import java.io._
      val EOF = -1
      trait MyBufferedInputStream extends java.io.InputStream with ConsoleLogger with ActivatableLogger {
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
            log(s"The $count-bytes buffer '${buf.take(count).map(_.toChar).mkString}' " +
              s"has been exhausted; trying to read more bytes...") // for demo
            pos = 0
            val nrBytesRead = read(buf)
            if (nrBytesRead > 0) {
              count = nrBytesRead
              nextByte()
            }
            else {
              log("The input stream has finished.")
              count = 0
              EOF
            }
          }
        }
      }
      // demo
      val filepath = "./src/main/scala/com/stefperf/impatient/Chapter10/"
      val filename = "Chapter10.txt"
      val bufferSize = 10
      println("-- Echoing file $filename one character at a time, but reading bufferSize characters at a time... --")
      val in = new java.io.FileInputStream(new File(filepath + filename)) with MyBufferedInputStream {
        val bufSize: Int = bufferSize
        var doLog = true
      }
      var hasNext = true
      while (hasNext) {
        val next = in.read()
        if (next == EOF) hasNext = false
        else println(next.toChar)
      }
    }

    exercise(11) {
      import java.io._
      abstract class IterableInputStream extends InputStream with Iterable[Byte] {
        class InputStreamIterator(private val in: InputStream) extends Iterator[Byte] {
          protected val EOF: Int = -1 // end of stream reached
          protected[this] var nextByte: Option[Byte] = None

          def hasNext: Boolean = {
            if (nextByte.isDefined) true
            else {
              val readResult = in.read()
              if (readResult == EOF) false
              else {
                nextByte = Some(readResult.toByte)
                true
              }
            }
          }

          def next(): Byte = {
            if (hasNext) {
              val nextByteValue = nextByte.get
              nextByte = None
              nextByteValue
            } else {
              throw new IOException("no next Byte can be accessed from the input stream")
            }
          }
        }

        override def iterator: Iterator[Byte] = new InputStreamIterator(this)
      }
      val filepath = "./src/main/scala/com/stefperf/impatient/Chapter10/"
      val filename = "Chapter10.txt"
      val iterableInputStream = new IterableInputStream {
        val in = new java.io.FileInputStream(new File(filepath + filename))

        override def read(): Int = in.read()
      }
      val streamIterator = iterableInputStream.iterator
      println("-- Echoing file $filename one character at a time with iterator... --")
      while (streamIterator.hasNext) println(streamIterator.next().toChar)
    }

    exercise(12) {
      println("Skipped as it is not a coding exercise.")
    }

  }
}
