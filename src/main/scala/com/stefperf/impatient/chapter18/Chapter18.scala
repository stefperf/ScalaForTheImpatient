package com.stefperf.impatient.chapter18

import com.stefperf.impatient._

object Chapter18 extends Chapter(18, "Type Parameters", Level.L2) {

  override def exercises() {

    exercise(1) {
      case class Pair[T, U](t: T, u: U) {
        def swap: Pair[U, T] = Pair[U, T](u, t)

        override def toString: String = s"<$t, $u>"
      }

      val pair = Pair("pair", 1.0)
      println(s"pair = $pair, pair.swap = ${pair.swap}")
    }

    exercise(2) {
      class Pair[T](var t1: T, var t2: T) {
        def swap(): Unit = {
          val temp = t1
          t1 = t2
          t2 = temp
        }

        override def toString: String = s"<$t1, $t2>"
      }

      val pair = new Pair(1, 2)
      println(s"pair before pair.swap(): $pair")
      pair.swap()
      println(s"pair after pair.swap():  $pair")
    }

    exercise(3) {
      class Pair[T, U](var t: T, var u: U) {
        override def toString: String = s"<$t, $u>"
      }

      def swap[T, U](pair: Pair[T, U]): Pair[U, T] = new Pair(pair.u, pair.t)

      val pair = new Pair("pair", 1.0)
      println(s"pair = $pair, swap(pair) = ${swap(pair)}")
    }

    exercise(4) {
      class Person(val name: String)

      class Student(_studentName: String, val universityId: Int) extends Person(_studentName)

      case class Pair[T](first: T, second: T) {
        def replaceFirst(newFirst: T) = new Pair[T](newFirst, second)
      }

      val personPair = Pair(new Person("Stefano"), new Person("Luca"))
      val studentStefano = new Student("Stefano", 632089)
      println(s"personPair before personPair.replaceFirst(studentStefano) = $personPair")
      personPair.replaceFirst(studentStefano)
      println(s"personPair after personPair.replaceFirst(studentStefano) =  $personPair")
      println("This replacement causes no issues because a Student is a (subclass of) Person.")
    }

    exercise(5) {
      println(s"RichInt implements Comparable[Int] instead of Comparable[RichInt] so that it doesn't need to convert " +
        s"the argument of method compareTo from Int to RichInt, which is more efficient.")
    }

    exercise(6) {
        def middle[T](iterable: Iterable[T]): T = {
          require(iterable.nonEmpty, "argument 'iterable' must have at least one element")
          val desiredIndex = iterable.size / 2 - (if (iterable.size % 2 == 0) 1 else 0)
          val it = iterable.iterator
          (0 until desiredIndex).foreach(_ => it.next())
          it.next()
        }

      println("middle(\"world\") = " + middle("world"))
      println("middle(List(1, 2, 3, 4)) = " + middle(List(1, 2, 3, 4)))
    }

    exercise(7) {
      println("Iterable[+A] is contravariant in A because it has type A as the output type of its output-producing methods.")
    }

    exercise(8) {
      println("A mutable generic container cannot be covariant in its element type because that is an input type to its setter methods.")
    }

    exercise(9) {
      val text =
        """// luckily, this code is illegal!
          |class Pair[+T](val first: T, val second: T) {
          |  def replaceFirst(newFirst: T): Pair[T] = { new Pair(newFirst, second) }
          |}
          |
          |class NastyDoublePair(_first: Double, _second: Double) extends Pair[Double](_first, _second) {
          |  override def replaceFirst(newFirst: Double): Pair[Double] = new Pair[Double](Math.sqrt(newFirst), second)
          |}
          |
          |val pair: Pair[Any] = new NastyDoublePair(3.14, 2.718)
          |pair.replaceFirst(\"Hello\")  // this line would throw an IllegalArgumentException
          |""".stripMargin
      println(text)
    }

    exercise(10) {
      class Pair[S, T](var s: S, var t: T) {
        // this trick was the only way I found to let the implicit evidence for the type constraint be found
        def swap(unusedParameterJustAsATrickToLetImplicitBeFound: S = s)(implicit ev: S =:= T): Unit = {
          val u = s
          s = t.asInstanceOf[S]
          t = u.asInstanceOf[T]
        }

        override def toString: String = s"<$s, $t>"
      }

      val pair = new Pair(3.14, 2.718)
      println(s"pair before swap = $pair")
      pair.swap()
      println(pair)
      println(s"pair after swap =  $pair")
    }

  }
}
