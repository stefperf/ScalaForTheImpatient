package com.stefperf.impatient.chapter19

import com.stefperf.impatient._
import scala.language.existentials

object Chapter19 extends Chapter(19, "Advanced Types", Level.L2) {

  class Bug {
    private var _position = 0
    def position: Int = _position
    private var _direction = 1
    def direction: Int = _direction

    def move(steps: Int): Bug = {
      _position += steps * _direction
      this
    }

    def turn(): Bug = {
      _direction *= -1
      this
    }

    def show(): Bug = {
      print(s"$position ")
      this
    }
  }

  class FluentBug extends Bug {

    override def move(steps: Int): FluentBug = super.move(steps).asInstanceOf[FluentBug]
    override def turn(): FluentBug = super.turn().asInstanceOf[FluentBug]
    def turn(_around: FluentBugCommands.around.type): FluentBug = turn
    override def show(): FluentBug = super.show().asInstanceOf[FluentBug]
    def and(_show: FluentBugCommands.show.type): FluentBug = this.show
    def and(_then: FluentBugCommands.then.type): FluentBug = this
  }

  class FluentBugCommandObject
  object FluentBugCommands {
    object show extends FluentBugCommandObject
    object then extends FluentBugCommandObject
    object around extends FluentBugCommandObject
  }

  override def exercises() {

    exercise(1) {
      val bugsy = new Bug
      bugsy.move(4).show().move(6).show().turn().move(5).show()
    }

    exercise(2) {
      import com.stefperf.impatient.chapter19.Chapter19.FluentBugCommands._
      val bugsy = new FluentBug
      bugsy move 4 and show and then move 6 and show turn around move 5 and show
    }

    exercise(3) {
      object Title
      object Author

      class Document {
        private var useNextArgAs: Any = null

        private var _title: String = _

        def title: String = _title

        private var _author: String = _

        def author: String = _author

        def set(obj: Title.type): this.type = {
          useNextArgAs = obj; this
        }

        def set(obj: Author.type): this.type = {
          useNextArgAs = obj; this
        }

        def to(arg: String): this.type = {
          if (useNextArgAs == Title) _title = arg
          else if (useNextArgAs == Author) _author = arg
          this
        }

        override def toString: String = s"Title: '${_title}', Author: '${_author}'"
      }

      val book = new Document
      book set Title to "Scala for the Impatient" set Author to "Cay Horstmann"
      println(book)
    }

    import scala.collection.mutable.ArrayBuffer
    class Network {
      private var _id = Network.getUniqueId

      def id: Int = _id

      class Member(val name: String, val network: Network) {
        val contacts = new ArrayBuffer[Member]

        final override def equals(obj: Any): Boolean = obj match {
          case other: Member => this.name == other.name
          case _ => false
        }

        override def toString: String = s"network_${network.id}.${name}"
      }

      private val members = new ArrayBuffer[Member]

      def join(name: String): Member = {
        val m = new Member(name, this)
        members += m
        m
      }

      override def toString: String = s"network_${id}"
    }

    object Network {
      private var _count = 0

      private def getUniqueId: Int = {
        val id = _count
        _count += 1
        id
      }
    }

    val tvitter = new Network
    val facebuk = new Network
    val stefanoOnTvitter = tvitter.join("Stefano")
    val stefanoOnTvitter2 = stefanoOnTvitter
    val stefanoOnFacebuk = facebuk.join("Stefano")
    val rileyOnFacebuk = facebuk.join("Riley")

    exercise(4) {
      println(s"$stefanoOnTvitter == $stefanoOnFacebuk: ${stefanoOnTvitter == stefanoOnFacebuk}")
      println(s"$stefanoOnTvitter == $stefanoOnTvitter2: ${stefanoOnTvitter == stefanoOnTvitter2}")
    }

    exercise(5) {
      type NetworkMember = n.Member forSome {val n: Network}

      def process(m1: NetworkMember, m2: NetworkMember): (NetworkMember, NetworkMember) = (m1, m2)

      val pairFromDifferentNetworks = process(stefanoOnTvitter, rileyOnFacebuk)
      println(s"In this case, function process works also for members of distinct networks, " +
        s"such as ${stefanoOnTvitter} and ${rileyOnFacebuk}.")
    }

    exercise(6) {
      type ExactMatchIndex = Int
      type ClosestMatchIndex = Int

      // expects a sorted array, but does not check for sortedness
      def indexOfClosestElement(uncheckedAscendingElements: Array[Int], elementToSearch: Int): ClosestMatchIndex Either ExactMatchIndex =
        if (uncheckedAscendingElements.isEmpty) Left(-1)
        else {
          val maybeFirstLargerOrEqualElemAndIndex = uncheckedAscendingElements.zipWithIndex.find(_._1 >= elementToSearch)
          if (maybeFirstLargerOrEqualElemAndIndex.isEmpty) Left(uncheckedAscendingElements.indices.last)
          else {
            val (firstLargerOrEqualElem, firstLargerOrEqualIndex) = maybeFirstLargerOrEqualElemAndIndex.get
            if (firstLargerOrEqualElem == elementToSearch) Right(firstLargerOrEqualIndex)
            else if (firstLargerOrEqualIndex == 0) Left(0)
            else {
              val firstLowerIndex = firstLargerOrEqualIndex - 1
              val firstLowerElem = uncheckedAscendingElements(firstLowerIndex)
              if (elementToSearch - firstLowerElem <= firstLargerOrEqualElem - elementToSearch) Left(firstLowerIndex)
              else Left(firstLargerOrEqualIndex)
            }
          }
        }

      val sortedNumbers = Array(1, 4, 6)
      println(s"Searching for the closest position within array (${sortedNumbers.mkString(", ")}):")
      for (numberToSearch <- Seq(0, 3, 4, 5, 7)) {
        val closestIndex = indexOfClosestElement(sortedNumbers, numberToSearch)
        closestIndex match {
          case Right(exactMatchIndex) => println(s"$numberToSearch is found in position $exactMatchIndex")
          case Left(closestMatchIndex) => println(s"$numberToSearch is closest to position $closestMatchIndex")
        }
      }
    }

    exercise(7) {
      import scala.io.{Source, BufferedSource}

      def processAndThenClose[T](closable: {def close(): Unit}, process: Any => T): Either[Throwable, T] = {
        try {
          val result = process(closable)
          Right(result)
        }
        catch {
          case t: Throwable => Left(t)
        }
        finally {
          closable.close()
          println("The closable object was closed.")
        }
      }

      val filePathname = "./src/main/scala/com/stefperf/impatient/chapter19/Chapter19.scala"
      val file = Source.fromFile(filePathname)
      val first10Chars = processAndThenClose(file, (aFile: Any) => {
        val file = aFile.asInstanceOf[BufferedSource]
        val nchars = 10
        val cbuf = new Array[Char](nchars)
        file.reader().read(cbuf, 0, nchars)
        cbuf.mkString
      })
      println(s"first 10 characters = '$first10Chars'")
      val deliberateFailure = processAndThenClose(file, (aFile: Any) => {
        val file = aFile.asInstanceOf[BufferedSource]
        val nchars = 10000
        val cbuf = new Array[Char](nchars)
        file.reader().read(cbuf, 0, nchars)
        cbuf.mkString
      })
      println(s"deliberate failure = '$deliberateFailure'")
    }

    exercise(8) {
      type ApplicableInt = {def apply(i: Int): Int}

      val printValues: (ApplicableInt, Int, Int) => Unit =
        (f: ApplicableInt, from: Int, to: Int) => {
          Range(from, to).foreach(i => print(s"${f(i)} "))
        }
      val integers = Seq(0, 1, 2, 3, 4, 5)
      printValues(integers, 2, 4)
    }

    exercise(9) {
      abstract class Dim[T](val value: Double, val name: String) {
        this: T =>
        protected def create(v: Double): T

        def +(other: Dim[T]) = create(value + other.value)

        override def toString() = s"$value $name"
      }

      class Seconds(v: Double) extends Dim[Seconds](v, "s") {
        this: Dim[Seconds] =>
        override def create(v: Double) = new Seconds(v)
      }

      println("Self type 'this: T =>' in the BaseClass Dim enforces " +
        "the type parameter to be the same as the subclass in each subclass.")
    }

    exercise(10) {
      trait TraitPrintingSomething {
        println("Initializing TraitPrintingSomething...")

        def printSomething(): Unit = println("Printing something...")
      }

      abstract class ClassWithSelfTypeTraitPrintingSomething { this: TraitPrintingSomething =>
        println("Initializing ClassWithSelfTypeTraitPrintingSomething...")
        printSomething()
      }

      new ClassWithSelfTypeTraitPrintingSomething with TraitPrintingSomething
      println()

      class ClassPrintingSomething {
        println("Initializing ClassPrintingSomething...")

        def printSomething(): Unit = println("Printing something...")
      }

      trait TraitExtendingClassPrintingSomething extends ClassPrintingSomething {
        println("Initializing TraitExtendingClassPrintingSomething...")
        printSomething()
      }

      new ClassPrintingSomething() with TraitExtendingClassPrintingSomething()
      println()
    }
  }
}
