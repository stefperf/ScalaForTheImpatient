package com.stefperf.impatient.chapter11

import scala.annotation.tailrec

class Fraction(n: Int, d: Int) {
  import Fraction.{gcd, mcm}
  private val num: Int = if (d == 0) 1 else n * Fraction.sign(d) / gcd(n, d)
  private val den: Int = if (d == 0) 0 else d * Fraction.sign(d) / gcd(n, d)
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
  def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)
  def mcm(a: Int, b: Int): Int = a * b / gcd(a, b)
}
