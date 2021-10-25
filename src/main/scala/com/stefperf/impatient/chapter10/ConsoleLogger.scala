package com.stefperf.impatient.chapter10

trait ConsoleLogger {
  val lineStart = "### "
  def log(msg: String) { println(lineStart + msg) }
}