package com.stefperf.impatient.chapter11

// this implementation could benefit a lot from implicit conversions b/w Int and Bit, Int and BitIndex
sealed abstract case class Bit(value: Int) {
  override def toString: String = value.toString
}
object Zero extends Bit(0)
object One extends Bit(1)

object Bit {
  def apply(i: Int): Bit = {
    i match {
      case 0 => Zero
      case 1 => One
      case _ => throw new IllegalArgumentException("i must be 0 or 1")
    }
  }
}

case class BitIndex(value: Int) {
  import BitIndex._
  require(MIN <= value && value <= MAX, s"`value` must be in range [$MIN, $MAX]")
}

object BitIndex {
  val MIN = 0
  val N_VALUES = 64
  val MAX: Int = N_VALUES - 1
  val RANGE: Seq[BitIndex] = (MIN to MAX).map(apply)
}

final case class BitSequence private (private var _bits: Long = 0L) {
  def bits: Long = _bits

  private def mask(index: BitIndex): Long = 1L << index.value

  def apply(index: BitIndex): Bit = Bit(((_bits & mask(index)) >> index.value).toInt)

  def apply(index: Int): Bit = apply(BitIndex(index))

  def update(index: BitIndex, newBit: Bit) {
    newBit match {
      case Zero => _bits &= ~mask(index)
      case One => _bits |= mask(index)
    }
  }

  def update(index: Int, newBit: Bit): Unit = update(BitIndex(index), newBit)

  override def toString: String = BitIndex.RANGE.map(apply).mkString
}

object Chapter11Exercise07 extends App {
  val bitSeq = BitSequence()
  println(s"initial BitSequence contents: $bitSeq")
  (BitIndex.MIN until BitIndex.MAX by 2).foreach { bitSeq(_) = One }
  (BitIndex.MIN until BitIndex.MAX by 3).foreach { bitSeq(_) = Zero }
  println(s"final BitSequence contents:   $bitSeq")
}
