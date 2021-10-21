package com.stefperf.impatient.chapter08

import com.stefperf.impatient.{Chapter, exercise, seq2line}

object Chapter08 extends Chapter(8, "Inheritance", {
  exercise(1) {
    class BankAccount(initialBalance: Double) {
      private var balance = initialBalance

      def currentBalance = balance

      def deposit(amount: Double) = {
        balance += amount; balance
      }

      def withdraw(amount: Double) = {
        balance -= amount; balance
      }
    }

    // bank's own account for fees received
    val feesAccount = new BankAccount(0.0)

    // checking account
    class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
      val depositFee, withdrawalFee = 1.0

      override def deposit(amount: Double) = {
        super.withdraw(depositFee)
        feesAccount.deposit(depositFee)
        super.deposit(amount)
      }

      override def withdraw(amount: Double) = {
        super.withdraw(withdrawalFee)
        feesAccount.deposit(withdrawalFee)
        super.withdraw(amount)
      }
    }

    val checkingAccount = new CheckingAccount(0.0)
    checkingAccount.deposit(10000.0)
    checkingAccount.withdraw(1000.0)
    checkingAccount.withdraw(1000.0)
    println("After 3 operations:")
    println(f"- the checking account balance is ${checkingAccount.currentBalance}")
    println(f"- the fee total received by the bank is ${feesAccount.currentBalance}")
  }

  exercise(3) {
    println("Source: https://www.tutorialspoint.com/cplusplus/cpp_inheritance.htm (not a great example, in truth)")

    class Shape {
      protected var width: Int = 0
      protected var height: Int = 0

      def setWidth(value: Int) = width = value

      def setHeight(value: Int) = height = value
    }

    class Rectangle extends Shape {
      def area = width * height
    }

    val rect = new Rectangle
    rect.setWidth(5)
    rect.setHeight(7)
    println(f"Total area: ${rect.area}")
  }

  exercise(4) {
    abstract class Item {
      def description: String

      def price: Double
    }
    class SimpleItem(override val description: String, override val price: Double) extends Item
    class Bundle(initialItems: Item*) extends Item {
      protected var items: List[SimpleItem] = Nil
      initialItems foreach add

      def description: String = items.sortBy(-_.price).map(_.description).mkString(" + ")

      def price: Double = items.foldLeft(0.0)(_ + _.price)

      def add(item: Item) {
        item match {
          case simpleItem: SimpleItem => items = simpleItem :: items
          case bundle: Bundle => items = bundle.items ::: items
          case _ => throw new IllegalArgumentException(s"Items of class ${item.getClass} are not supported in bundles.")
        }
      }
    }
    val keyboard = new SimpleItem("keyboard", 39.9)
    val mouse = new SimpleItem("mouse", 49.9)
    val mousePad = new SimpleItem("mouse pad", 9.9)
    val mouseBundle = new Bundle(mouse, mousePad)
    val accessories = new Bundle(keyboard, mouseBundle)
    println(f"The bundle '${accessories.description}' costs ${accessories.price}%.2f CHF.")
  }

  exercise(5) {
    class Point(val x: Double, val y: Double) {
      override def toString: String = f"Point($x, $y)"
    }
    class LabeledPoint(val label: String, x: Double, y: Double) extends Point(x, y) {
      override def toString: String = f"LabeledPoint('$label', $x, $y)"
    }
    println(new Point(3, 4))
    println(new LabeledPoint("Black Thursday", 1929, 230.07))
  }

  exercise(6) {
    // x axis increases from left to right, y axis from top to bottom, as is natural on a webpage
    class Point(val x: Double, val y: Double) {
      override def toString: String = f"Point($x, $y)"
    }
    abstract class Shape {
      def centerPoint: Point
    }
    class Rectangle(val topLeftCorner: Point, val width: Double, val height: Double) extends Shape {
      def centerPoint: Point = new Point(topLeftCorner.x + width / 2, topLeftCorner.y + height / 2)

      override def toString: String = f"Rectangle(topLeftCorner=$topLeftCorner, width=$width, height=$height)"
    }
    class Circle(val centerPoint: Point, val radius: Double) extends Shape {
      override def toString: String = f"Rectangle(centerPoint=$centerPoint, radius=$radius)"
    }
    val shapes = Array[Shape](
      new Rectangle(new Point(3, 4), 7, 10),
      new Circle(new Point(5, 5), 5)
    )
    for (shape <- shapes; centerPoint = shape.centerPoint) println(f"$shape => center$centerPoint.")
  }

  exercise(7) {
    class Point(val x: Int, val y: Int) {
      override def toString: String = f"Point($x, $y)"
    }
    import java.awt.Rectangle
    class Square(var topLeftCorner: Point, side: Int)
      extends Rectangle(topLeftCorner.x, topLeftCorner.y, side, side) {
      def this(side: Int) = this(new Point(0, 0), side)

      def this() = this(new Point(0, 0), 0)

      override def toString = f"Square(topLeftCorner=$topLeftCorner, side=$width)"
      // more work would be needed to insure that width and height always stay equal
    }
    println(f"new Square(new Point(3, 4), 5) => ${new Square(new Point(3, 4), 5)}")
    println(f"new Square(5) => ${new Square(5)}")
    println(f"new Square() => ${new Square()}")
  }

  exercise(8) {
    println("(bytecode reverse engineering exercise) the name field is duplicated, the getter method is overridden.")
  }

  exercise(9) {
    class Creature {
      def range: Int = 10

      val env: Array[Int] = new Array[Int](range)
    }
    class AntWithVal extends Creature {
      override val range = 2
    }
    class AntWithDef extends Creature {
      override def range = 2
    }
    println(f"new AntWithVal().env = ${seq2line(new AntWithVal().env)}, unwanted!")
    println(f"new AntWithDef().env = ${seq2line(new AntWithDef().env)}, as desired.")
    println("The 2nd case works correctly because it is de facto a static method, so it needs no initialization.")
  }

  exercise(10) {
    println("(theory answer)\n" +
      "A 'protected' constructor can be called only by this class (+companion object) and its subclasses.")
  }

  exercise(11) {
    Chapter08Exercise11.main(Array.empty)
  }

})
