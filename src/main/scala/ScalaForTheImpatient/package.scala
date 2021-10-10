import scala.collection.mutable.ArrayBuffer

package object ScalaForTheImpatient {

  val LineLength = 120

  def fillLine(line: String, filler: String, lineLength: Int = LineLength): String =
    line + filler.head.toString * (LineLength - line.length)

  def exercise(numbers: Int*)(body: => Unit): Unit = {
    if (numbers.isEmpty) throw new IllegalArgumentException("At least 1 exercise number must be input.")
    val nums = numbers.map(_.toString).mkString(", ")
    val singularOrPlural = if (numbers.length > 1) Array("s", " together") else Array("", "")
    val FillCh = "-"
    println(fillLine(f"${FillCh * 3} Exercise${singularOrPlural(0)} # $nums${singularOrPlural(1)} ", FillCh))
    body
    println()
  }

  def seq2line[T](seq: Iterable[T]): String = seq.iterator.map(_.toString).mkString(start = "(", sep = ", ", end = ")")

  def printseqline[T](seq: Iterable[T], start: String = "", end: String = "\n"): Unit =
    print(start + seq2line(seq) + end)

  def map2str[K, V](map: scala.collection.Map[K, V], lineStart: String = "  ", sep: String = ",\n"): String =
    (for ((k, v) <- map) yield f"$lineStart($k => $v)").mkString(sep)

  def printmap[K, V](map: scala.collection.Map[K, V], lineStart: String = "  ", sep: String = ",\n",
                     start: String = "", end: String = "\n)\n"): Unit =
    print(start + "(\n" + map2str(map, lineStart, sep) + end)

  object Chapter {
    private val FillCh = "="
    private val chaptersByNumber = scala.collection.mutable.LinkedHashMap[Int, Chapter]()
    protected def header(number: Int, title: String) = f"Chapter $number. $title"
    protected def frameBody(number: Int, title: String, body: => Unit): Unit = {
      println(FillCh * LineLength)
      println(fillLine(f"${FillCh * 3} ${header(number, title)} ", FillCh))
      println()
      body
      println()
      println()
    }
    def execute(numbers: Int*): Unit = {
      val numsRequested = if (numbers.nonEmpty) numbers else chaptersByNumber.keys
      val headersExecuted = ArrayBuffer[String]()
        for (num <- numsRequested) {
          if (chaptersByNumber.contains(num)) {
            val chapter = chaptersByNumber(num)
            chapter.main(Array.empty)
            headersExecuted += chapter.header
          }
          else frameBody(num, "", {println("not isImplemented")})
        }
      if (headersExecuted.nonEmpty) {
        println(FillCh * LineLength)
        println(fillLine(FillCh * 3 + " Chapters executed: ", FillCh))
        headersExecuted foreach {println(_)}
      }
    }
    def isImplemented(number: Int): Boolean = chaptersByNumber.contains(number)
  }

  class Chapter (val number: Int, title: String, body: => Unit) {
    Chapter.chaptersByNumber(number) = this  // register this chapter
    def header: String = Chapter.header(number, title)
    def main(args: Array[String]): Unit = Chapter.frameBody(number, title, body)
    def forceInit(): Unit = {}  // just trigger construction execution
  }

}
