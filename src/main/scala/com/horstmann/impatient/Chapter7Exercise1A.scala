package com {
  package horstmann {
    class Something { def print(): Unit = {println("This is class com.horstmann.Something.")}}
    package impatient {
      class Something { def print(): Unit = {println("This is class com.horstmann.impatient.Something.")}}
      object Chapter7Exercise1A extends App {
        println("Object Chapter7Exercise1A can see both com.horstmann and com.horstmann.impatient members:")
        new horstmann.Something().print()
        new impatient.Something().print()
      }
    }
  }
}







