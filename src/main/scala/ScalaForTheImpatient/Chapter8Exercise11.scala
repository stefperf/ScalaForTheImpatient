package ScalaForTheImpatient

object Chapter8Exercise11 extends App {

  class Point private(private val xy: Long) extends AnyVal {
    final def x: Int = ((xy & 0xFFFFFFFF00000000L) >> 32).toInt

    final def y: Int = (xy & 0x00000000FFFFFFFFL).toInt

    override def toString = f"Point(x = $x, y = $y)"
  }

  object Point {
    // convenience method for easier initialization of value class Point
    final def apply(x: Int, y: Int) =
      // care must be taken to avoid sign extension of negative y's; there may be better ways to do it
      new Point((x.toLong << 32) | (if (y >= 0) y.toLong else y.toLong & 0x00000000FFFFFFFFL | 0x0000000080000000L))
  }

  for (x <- Seq(-3, +5); y <- Seq(-4, +2); p = Point(x, y)) println(p)
}