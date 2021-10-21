package com.stefperf.impatient.chapter07

package object random {

  import scala.math.pow

  private val a = 1664525
  private val b = 1664525
  private val n = 32
  private val m = pow(2, n).toInt
  private var previous: Int = 0

  def setSeed(seed: Int): Unit = {
    previous = seed
  }

  def nextInt(): Int = {
    previous = (previous * a + b) % m
    previous
  }

  // random Double in [-1, +1]
  def nextDouble(): Double = nextInt().toDouble / Int.MaxValue
}
