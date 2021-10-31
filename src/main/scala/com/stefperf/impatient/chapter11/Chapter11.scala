package com.stefperf.impatient.chapter11

import com.stefperf.impatient._

object Chapter11 extends Chapter(11, "Operators", {
  exercise(1) {
    println(s"3 + 4 -> 5 == (3 + 4) -> 5 == ${3 + 4 -> 5}, " +
      s"from left to right, because both operators have the same precedence.")
    println(s"3 + 4 -> 5 is legal only as 3 -> (4 + 5) == ${3 -> (4 + 5)}.")
  }

  exercise(2) {
    println("Operators ** or ^ for power would have an unintuitively too low precedence, based on Scala's rules.")
  }

  exercise(3) {
    Chapter11Exercise03.main(Array.empty)
  }

  exercise(4) {
    Chapter11Exercise04.main(Array.empty)
 }

  exercise(5) {
    Chapter11Exercise05.main(Array.empty)
  }

  exercise(6) {
    Chapter11Exercise06.main(Array.empty)
  }

  exercise(7) {
    Chapter11Exercise07.main(Array.empty)
  }

  exercise(8) {
    Chapter11Exercise08.main(Array.empty)
  }

  exercise(9) {
    object PathComponents {
      val SEP = '/'
      def unapply(path: String): Option[(String, String)] = {
        val p = path.trim
        if (p.isEmpty || p.contains(s"$SEP$SEP")) None  // do not match blank or invalid strings
        var i = p.length - 1
        while (i >= 0) {
          if (p(i) == SEP) return Some((p.take(i), p.drop(i + 1)))
          i -= 1
        }
        Some(("", p))
      }
    }
    val potentialPathnames = Seq("  ", "/home/cay/readme.txt", " readme.txt  ", "")
    for (ppn <- potentialPathnames)
      ppn match {
        case PathComponents(dir, filename)
          => println(s"String '$ppn' matches to <dir = '$dir', filename = '$filename'>.")
        case _
          => println(s"String '$ppn' does not match the target pattern.")
      }
  }

  exercise(10) {
    object PathComponents {
      val SEP = '/'
      def unapplySeq(path: String): Option[Seq[String]] = {
        val p = path.trim
        if (p.isEmpty || p.contains(s"$SEP$SEP")) None  // do not match blank or invalid strings
        else Some(p.split(SEP).filter(_.nonEmpty))
      }
    }
    val potentialPathnames = Seq("  ", "/home/cay/readme.txt", " readme.txt  ", "", "dirname/filename.ext")
    for (ppn <- potentialPathnames)
      ppn match {
        case PathComponents(filename)
          => println(s"String '$ppn' matches to <filename = '$filename'>.")
        case PathComponents(dir, filename)
          => println(s"String '$ppn' matches to <dir = $dir, filename = '$filename'>.")
        case PathComponents(dir1, dir2, filename)
          => println(s"String '$ppn' matches to <dir1 = $dir1, dir2 = $dir2, filename = '$filename'>.")
        case _ =>
          println(s"String '$ppn' does not match any of the target patterns.")
      }
  }

  exercise(11) {
    println("not implemented yet")
  }
  exercise(12) {
    println("not implemented yet")
  }
  exercise(13) {
    println("not implemented yet")
  }
})
