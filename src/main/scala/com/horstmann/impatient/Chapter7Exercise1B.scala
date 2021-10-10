package com.horstmann.impatient

object Chapter7Exercise1B extends App {
  println("Object Chapter7Exercise1B cannot see com.horstmann members, but only com.horstmann.impatient members:")
  new Something().print()
}
