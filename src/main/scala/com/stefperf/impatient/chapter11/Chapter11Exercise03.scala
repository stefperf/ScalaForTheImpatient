package com.stefperf.impatient.chapter11

import scala.annotation.tailrec

// this class could be improved by implementing interoperability with Int's
case class Fraction(n: Int, d: Int) {
  require(d != 0, "the denominator cannot be zero")
  def this(n: Int) = this(n, 1)
  import Fraction.{gcd, mcm}
  private val num: Int = n * Fraction.sign(d) / gcd(n, d)
  private val den: Int = d * Fraction.sign(d) / gcd(n, d)
  override def toString = s"$num/$den"
  def sign: Int = Fraction.sign(num)
  def +(that: Fraction): Fraction = {
    val newDen = mcm(this.den, that.den)
    val newNum = (for (f <- Seq(this, that)) yield newDen / f.den * f.num).sum
    new Fraction(newNum, newDen)
  }
  def -(that: Fraction): Fraction = this + that * new Fraction(-1, 1)
  def *(that: Fraction): Fraction = new Fraction(this.num * that.num, this.den * that.den)
  def /(that: Fraction): Fraction = this * new Fraction(that.den, that.num)
}

object Fraction {
  def sign(a: Int): Int = if (a > 0) 1 else if (a < 0) -1 else 0
  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)
  def mcm(a: Int, b: Int): Int = a * b / gcd(a, b)
}

object Chapter11Exercise03 extends App {
  def parenNeg(f: Fraction) = if (f.sign < 0) s"($f)" else f.toString
  val f1 = new Fraction(2, 3)
  for (
    f2 <- Seq(new Fraction(-3, -4), new Fraction(3, -4));
    (f1_operator, symbol) <- Seq(f1.+ _, f1.- _, f1.* _, f1./ _) zip Seq('+', '-', '*', '/')
  )
    println(s"$f1 $symbol ${parenNeg(f2)} = ${f1_operator(f2)}")
}