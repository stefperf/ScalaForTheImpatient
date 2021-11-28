package com.stefperf.impatient.chapter05

import com.stefperf.impatient._

object Chapter05 extends Chapter(5, "Classes", Level.A1) {
  override def exercises() {

    exercise(1) {
      class Counter(private var value: Int = 0) {
        def increment(): Unit = {
          value += 1
        }

        def current: Int = value
      }
      class SaferCounter(private var value: Int = 0) {
        def increment(): Unit = {
          if (value < Int.MaxValue) value += 1
          else throw new ArithmeticException("integer overflow prevented")
        }

        def current: Int = value
      }
      val counter = new Counter(Int.MaxValue)
      val saferCounter = new SaferCounter(Int.MaxValue)
      counter.increment()
      try saferCounter.increment()
      catch {
        case t: Throwable => println(f"saferCounter.increment() threw: '${t.getMessage}'")
      }
      println(f"counter -> ${counter.current}, saferCounter -> ${saferCounter.current}")
    }

    exercise(2) {
      class BankAccount {
        private var _balance: Double = 0.0

        def deposit(amt: Double): Unit = {
          require(amt > 0.0)
          _balance += amt
        }

        def withdraw(amt: Double): Unit = {
          require(amt > 0.0)
          require(_balance >= amt, "desired withdrawal exceeds balance")
          _balance -= amt
        }

        def balance: Double = _balance
      }
      val ba = new BankAccount
      ba.deposit(10000)
      ba.withdraw(4000)
      println(f"current bank account balance = ${ba.balance}")
    }

    exercise(3) {
      class Time(val hrs: Int, val min: Int) {
        require(0 <= hrs && hrs <= 23 && 0 <= min && min <= 59)

        def before(other: Time): Boolean = hrs <= other.hrs || hrs == other.hrs && min < other.min

        override def toString: String = f"$hrs%02d:$min%02d"
      }
      val t0 = new Time(min = 45, hrs = 9)
      val t1 = new Time(10, 5)
      println(f"$t0 before $t1 => ${t0 before t1}")
    }

    exercise(4) {
      class Time(hrs: Int, min: Int) {
        private val totmin: Int = hrs * 60 + min
        require(0 <= totmin && totmin <= 24 * 60 - 1)

        override def toString: String = f"$hrs%02d:$min%02d"

        def before(other: Time): Boolean = totmin <= other.totmin
      }
      val t0 = new Time(min = 45, hrs = 9)
      val t1 = new Time(10, 5)
      println(f"$t0 before $t1 => ${t0 before t1}")
    }

    exercise(5) {
      import scala.beans.BeanProperty
      class Student(@BeanProperty var name: String, @BeanProperty var id: Long)
      // both Scala-standard and Java-standard getters and setters are generated
      val s = new Student("Stefano", 17)
      s.setId(13)
      println(s.getId)
      // you can use the Java-standard methods also from Scala, but the code is more readable if you don't
    }

    exercise(6) {
      class Person(inputAge: Int = 0) {
        private var privateAge = if (inputAge >= 0) inputAge else 0

        def age: Int = privateAge

        def age_=(newValue: Int): Unit = {
          if (newValue > privateAge) privateAge = newValue; // Can't get younger }
        }
      }
      val inputAge = -3
      val p = new Person(inputAge)
      println(f"Person(inputAge = $inputAge).age = ${p.age}")
    }

    exercise(7) {
      class Person(fullName: String) { // plain parameter, because it does not make sense to store it
        private val nameParts = fullName.split(" ")
        require(nameParts.length == 2, "only 1 one-word name and 1 one-word surname are supported")

        def firstName: String = nameParts(0)

        def lastName: String = nameParts(1)

        override def toString: String = f"$firstName $lastName"
      }
      val p = new Person("Stefano Perfetti")
      println(f"first name = ${p.firstName}")
      println(f"last name = ${p.lastName}")
    }

    exercise(8) {
      // no need for multiple constructors!
      class Car(val manufacturer: String, val modelName: String,
                val modelYear: Int = -1, var licensePlate: String = "") {
        override def toString: String = f"$manufacturer $modelName $modelYear $licensePlate"
      }
      println(new Car("Tesla", "Model T"))
      println(new Car("Tesla", "Model T", 2021))
      println(new Car("Tesla", "Model T", licensePlate = "1234567"))
      println(new Car("Tesla", "Model T", 2021, "1234567"))
    }

    exercise(10) {
      // horribly long!
      class Employee {
        private var _name: String = "John Q. Public"

        def name: String = _name

        var salary: Double = 0.0

        def this(inputName: String, inputSalary: Double) {
          this()
          _name = inputName
          salary = inputSalary
        }

        override def toString: String = f"name = $name, salary = $salary"
      }
      val e0 = new Employee
      val e1 = new Employee("Stefano", 10.0)
      e1.salary = 10000
      for (e <- List(e0, e1)) println(f"e: $e")
    }
  }
}
