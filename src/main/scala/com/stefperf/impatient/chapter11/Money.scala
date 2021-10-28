package com.stefperf.impatient.chapter11

sealed case class Money private(dollars: Long, cents: Int) {
  import Money._
  require(0 <= cents && cents < centsToDollars, "cents argument is out of bounds")
  val decimalValue: Double = dollars.toDouble + cents.toDouble / centsToDollars
  def +(that: Money): Money = fromDecimalValue(this.decimalValue + that.decimalValue)
  def -(that: Money): Money = fromDecimalValue(this.decimalValue - that.decimalValue)
  def ==(that: Money): Boolean = this.dollars == that.dollars && this.cents == that.cents
  def <(that: Money): Boolean = this.dollars < that.dollars || this.dollars == that.dollars && this.cents < that.cents
  def multiply(m: Double): Money = Money.fromDecimalValue(m * decimalValue)
  def divide(m: Double): Money = Money.fromDecimalValue(m / decimalValue)
}

object Money {
  val centsToDollars = 100
  // def apply(dollars: Long, cents: Int): Money = new Money(dollars, cents)
  def fromDecimalValue(decimalValue: Double): Money = {
    val roundedCentsValue: Long = math.round(decimalValue * centsToDollars)
    Money(roundedCentsValue / centsToDollars, (roundedCentsValue % centsToDollars).toInt)
  }
  def ratio(num: Money, den: Money): Double = num.decimalValue / den.decimalValue
}