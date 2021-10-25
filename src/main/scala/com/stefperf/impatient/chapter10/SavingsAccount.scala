package com.stefperf.impatient.chapter10

class SavingsAccount(id: Int) extends Account(id) with ConsoleLogger {
  override val accountType: String = "SAVINGS ACCOUNT"
  override def deposit(amount: Double): Unit = {
    super.deposit(amount)
    log(s"Deposited $amount into account $id, new balance = $getBalance")
  }

  override def withdraw(amount: Double) {
    try {
      super.withdraw(amount)
      log(s"Withdrawn $amount from account $id, new balance = $getBalance")
    }
    catch {
      case _: Throwable => log(s"Failed to withdraw $amount from account $id, balance = $getBalance")
    }
  }
}
