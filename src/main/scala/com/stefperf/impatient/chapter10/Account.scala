package com.stefperf.impatient.chapter10

class Account(val id: Int) {
  private var balance = 0.0
  val accountType = "generic"
  def getBalance: Double = balance
  def deposit(amount: Double) { balance += amount }
  def withdraw(amount: Double) {
    if (amount <= balance) balance -= amount
    else throw new IllegalArgumentException("Attempted withdrawal amount exceeds account balance")
  }
}
