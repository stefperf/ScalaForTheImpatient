package com.stefperf.impatient.chapter11

case class ASCIIArt(inputRows: Seq[String]) {
  import ASCIIArt._
  require(inputRows.nonEmpty && inputRows.map(_.length).max > 0, "the inputRows cannot be all empty")
  val nRows: Int = inputRows.length
  val nCols: Int = inputRows.map(_.length).max
  private val _rows = inputRows.map(r => r + " " * (nCols - r.length)).toArray

  def rows: Array[String] = _rows

  override def toString: String = _rows.mkString("\n")

  def emptyRow: String = SEP * nCols

  def growNRowsCentered(newNRows: Int): ASCIIArt = {
    val (nRowsAbove, nRowsBelow) = splitExtra(nRows, newNRows)
    copy((1 to nRowsAbove).map(_ => emptyRow) ++ rows ++  (1 to nRowsBelow).map(_ => emptyRow))
  }
  
  def growNColsCentered(newNCols: Int): ASCIIArt = {
    val (nColsLeft, nColsRight) = splitExtra(nCols, newNCols)
    copy(rows.map(SEP * nColsLeft + _ + SEP * nColsRight))
  }
  
  // a << b: combine horizontally, with a to the left of b
  def <<(that: ASCIIArt): ASCIIArt = combineHorizontally(this, that)

  // a ^^ b: combine vertically, with a above b
  def ^^(that: ASCIIArt): ASCIIArt = combineVertically(this, that)
}

object ASCIIArt {
  // class settings
  val SEP = " "
  val N_SEPS = 1
  val HSEP: String = SEP * N_SEPS
  val MORE_AFTER = true

  def apply(text: String): ASCIIArt = new ASCIIArt(text.split("\n"))
  
  def splitExtra(m: Int, n: Int): (Int, Int) = {
    require(m <= n, "n cannot be greater than m")
    val diff = n - m
    val flooredHalf = diff / 2
    val maybeExtraOne = diff % 2
    if (MORE_AFTER) (flooredHalf, flooredHalf + maybeExtraOne)
    else (flooredHalf + maybeExtraOne, flooredHalf)
  }
  
  def equalizeNRowsCentered(a: ASCIIArt, b: ASCIIArt): (ASCIIArt, ASCIIArt) = {
    if (a.nRows < b.nRows) (a.growNRowsCentered(b.nRows), b)
    else if (a.nRows > b.nRows) (a, b.growNRowsCentered(a.nRows))
    else (a, b)
  }

  def equalizeNColsCentered(a: ASCIIArt, b: ASCIIArt): (ASCIIArt, ASCIIArt) = {
    if (a.nCols < b.nCols) (a.growNColsCentered(b.nCols), b)
    else if (a.nCols > b.nCols) (a, b.growNColsCentered(a.nCols))
    else (a, b)
  }

  def combineHorizontally(a: ASCIIArt, b: ASCIIArt): ASCIIArt = {
    val (a1, b1) = equalizeNRowsCentered(a, b)
    ASCIIArt((a1.rows zip b1.rows).map(t => t._1 + HSEP + t._2))
  }

  def combineVertically(a: ASCIIArt, b: ASCIIArt): ASCIIArt = {
    val (a1, b1) = equalizeNColsCentered(a, b)
    ASCIIArt(a1.rows ++ (1 to N_SEPS).map(_ => a1.emptyRow) ++ b1.rows)
  }
}

object Chapter11Exercise06 extends App {
  val a0 = ASCIIArt(
    """ /\_/\
      |( ' ' )
      |(  -  )
      | | | |
      |(__|__)""".stripMargin
  )
  val a1 = ASCIIArt(
    """   -----
      | / Hello \
      |<  Scala |
      | \ Coder /
      |   -----  """.stripMargin
  )
  println(s"a0 = \n$a0\n")
  println(s"a1 = \n$a1\n")
  println(
    """println(
      |  a0 << a1
      |     ^^
      |  a1 << a0
      |)""".stripMargin
  )
  println("\n -> \n")
  println(
    a0 << a1
       ^^
    a1 << a0
  )
}