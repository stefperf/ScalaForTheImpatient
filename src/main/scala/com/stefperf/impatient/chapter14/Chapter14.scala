package com.stefperf.impatient.chapter14

import com.stefperf.impatient._

object Chapter14 extends Chapter(14, "Pattern Matching and Case Classes") {
  override def exercises() {
    exercise(1) {
      "Skipped, as it is no programming exercise."
    }

    exercise(2) {
      def swap(intPair: (Int, Int)): (Int, Int) =
        intPair match {
          case (a, b) => (b, a)
        }

      val pair = (1, 2)
      println(s"swap($pair) = ${swap(pair)}")
    }

    exercise(3) {
      def swap(arr: Array[Int]): Array[Int] =
        arr match {
          case Array() | Array(_) => arr
          case Array(a, b, rest@_*) => Array(b +: a +: rest: _*)
        }

      for (n <- 0 to 3; arr = (1 to n).toArray)
        println(s"swap(${seq2line(arr)}) = ${seq2line(swap(arr))}")
    }

    exercise(4) {
      abstract class Item
      case class Article(description: String, price: Double) extends Item
      case class Bundle(description: String, discount: Double, items: Item*) extends Item
      case class Multiple(n: Int, item: Item) extends Item

      def price(it: Item): Double = it match {
        case Article(_, p) => p
        case Bundle(_, disc, its@_*) => its.map(price).sum - disc
        case Multiple(n, item) => n * price(item)
      }

      val article = Bundle("bundle", 3,
        Article("pizza", 16),
        Multiple(3, Article("French fries", 4))
      )
      println(s"price($article) = ${price(article)}")
    }

    exercise(5) {
      def leafSum(node: Any): Int = {
        (node: @unchecked) match {
          case i: Int => i
          case l: List[Any] => l.map(leafSum).sum
        }
      }

      val list = List(List(3, 8), 2, List(5))
      println(s"leafSum($list) = ${leafSum(list)}")
    }

    exercise(6) {
      sealed abstract class BinaryTree
      case class Leaf(value: Int) extends BinaryTree
      case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

      def leafSum(node: BinaryTree): Int = {
        node match {
          case Leaf(value) => value
          case Node(left, right) => leafSum(left) + leafSum(right)
        }
      }

      // the given example tree modified to be binary
      val binTree = Node(
        Node(Leaf(3), Leaf(8)),
        Node(Leaf(2), Leaf(5))
      )
      println(s"leafSum($binTree) = ${leafSum(binTree)}")
    }

    exercise(7) {
      sealed abstract class Tree
      case class Leaf(value: Int) extends Tree
      case class Node(children: Tree*) extends Tree

      def leafSum(node: Tree): Int = {
        node match {
          case Leaf(value) => value
          case Node(children@_*) => children.map(leafSum).sum
        }
      }

      val tree = Node(
        Node(Leaf(3), Leaf(8)),
        Leaf(2),
        Node(Leaf(5))
      )
      println(s"leafSum($tree) = ${leafSum(tree)}")
    }

    exercise(8) {
      sealed abstract class Tree
      case class Leaf(value: Int) extends Tree
      case class Op(op: String, children: Tree*) extends Tree

      // not a good idea; just for this exercise' sake and for fun
      def eval(node: Tree): Int = {
        (node: @unchecked) match {
          case Leaf(value) => value
          case Op("+", children@_*) => children.map(eval).sum
          case Op("*", children@_*) => children.map(eval).product
          case Op("-", child) => -eval(child)
          case Op("-", child1, child2) => eval(child1) - eval(child2)
          case Op("/", child1, child2) => eval(child1) / eval(child2)
          case Op("%", child1, child2) => eval(child1) % eval(child2)
          case Op("!", child) => (1 to eval(child)).product
          case Op(op, args@_*) =>
            throw new IllegalArgumentException(s"Operation '$op' with ${args.length} arguments is not supported")
        }
      }

      val tree0 = Op("+",
        Op("*", Leaf(3), Leaf(8)),
        Leaf(2),
        Op("-", Leaf(5))
      )
      val tree1 = Op("+",
        Op("!", Op("%", Leaf(8), Leaf(3))),
        Op("*", Leaf(2), Op("/", Leaf(19), Leaf(3)))
      )
      val tree2 = Op("-",
        Op("!", Op("%", Leaf(8), Leaf(3))),
        Op("*", Leaf(2), Op("/", Leaf(19), Leaf(3))),
        Leaf(1)
      )

      Seq(tree0, tree1, tree2).foreach { tree =>
        try {
          println(s"eval($tree) = ${eval(tree)}\n")
        }
        catch {
          case ex: Throwable => println(s"eval($tree) caused this error:\n  $ex\n")
        }
      }
    }

    exercise(9) {
      def sumVersion0(ois: List[Option[Int]]): Int = ois.flatten.sum
      def sumVersion1(ois: List[Option[Int]]): Int = ois.collect{ case Some(i) => i }.sum
      val funs = Map[String, List[Option[Int]] => Int]("sumVersion0" -> sumVersion0, "sumVersion1" -> sumVersion1)
      val args = Seq(List(Some(1), None, Some(2)), List(None, None), List())
      funs.foreach{ case (funName, fun) =>
        println(s"With $funName:")
        args.foreach{ ois => println(s"$funName($ois) = ${fun(ois)}") }
        println()
      }
    }

    exercise(10) {
      import scala.math.sqrt
      def f(x: Double) = if (x != 1) Some(1 / (x - 1)) else None

      def g(x: Double) = if (x >= 0) Some(sqrt(x)) else None

      def compose(externalFun: Double => Option[Double], internalFun: Double => Option[Double]): Double => Option[Double] =
        internalFun(_).fold(None: Option[Double])(externalFun)

      val h = compose(g, f) // h(x) is g(f(x))
      Seq(2, 1, 0).foreach { arg =>
        println(s"h($arg) = ${h(arg)}")
      }
    }
  }
}
