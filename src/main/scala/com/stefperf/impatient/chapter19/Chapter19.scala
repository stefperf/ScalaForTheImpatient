package com.stefperf.impatient.chapter19

import com.stefperf.impatient._

import java.lang.reflect.Member


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

        def set(obj: Title.type): this.type = { useNextArgAs = obj; this }

        def set(obj: Author.type): this.type = { useNextArgAs = obj; this }

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

    exercise(4) {

      import scala.collection.mutable.ArrayBuffer
      class Network {
        class Member(val name: String) {
          val contacts = new ArrayBuffer[Member]

          final override def equals(obj: Any): Boolean = obj match {
            case other: Member => this.name == other.name
            case _ => false
          }
        }

        private val members = new ArrayBuffer[Member]
        def join(name: String): Member = {
          val m = new Member(name)
          members += m
          m
        }
      }

      val tvitter = new Network
      val facebuk = new Network
      val stefanoOnTvitter = tvitter.join("Stefano")
      val stefanoOnTvitter2 = stefanoOnTvitter
      val stefanoOnFacebuk = facebuk.join("Stefano")
      println(s"stefanoOnTvitter == stefanoOnFacebuk: ${stefanoOnTvitter == stefanoOnFacebuk}")
      println(s"stefanoOnTvitter == stefanoOnTvitter2: ${stefanoOnTvitter == stefanoOnTvitter2}")

    }

    println("COMING SOON")
  }
}
