package com.stefperf.impatient.chapter17

import com.stefperf.impatient._
import Chapter17MiscUtils._

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration._

object Chapter17 extends Chapter(17, "Futures", Level.A2) {
  import scala.concurrent.ExecutionContext.Implicits.global

  val waitFutMsecs = 30  // default delay used in one future for all these exercises

  // compute fun within a future with given delay
  def delay[T, U](fun: T => U, msecs: Int = waitFutMsecs): T => Future[U] = (t: T) => Future { Thread.sleep(msecs); fun(t) }

  // return the value t within a future with given delay
  def delayed[T](t: T, msecs: Int = waitFutMsecs): Future[T] = Future { Thread.sleep(msecs); t }

  override def exercises() {

    exercise(1) {

      def delayedResult(retval: Int, sleepMillisecs: Int = waitFutMsecs): Int = {
        println(s"$threadAndTime with retval = $retval starting")
        Thread.sleep(sleepMillisecs)
        println(s"$threadAndTime with retval = $retval ending after about $waitFutMsecs millisecs")
        retval
      }

      val nTimes = 10
      println("-- Demonstrating behavior of given for loop:")
      (1 to nTimes).foreach { _ =>
        for (n1 <- Future { delayedResult(2) };
             n2 <- Future { delayedResult(40) })
          println(s"$threadAndTime: total = ${n1 + n2}")
        Thread.sleep(waitFutMsecs * 3)
        println()
      }

      println("-- Demonstrating behavior of translation with flatMap and map:")
      (1 to nTimes).foreach { _ =>
        Future { delayedResult(2) }.flatMap(
          n1 => Future { delayedResult(40) }.map(
            n2 => println(s"$threadAndTime: total = ${n1 + n2}")
          )
        )
        Thread.sleep(waitFutMsecs * 3)
        println()
      }

      println("The two futures and the combination of their results are executed sequentially in 3 different threads.")
    }

    exercise(2, 3) {
      def doInOrder[T, U, V](internalFun: T => Future[U], externalFun: U => Future[V]): T => Future[V] =
        t => for (u <- internalFun(t); output <- externalFun(u)) yield output

      def chainFutures[T](funs: (T => Future[T])*): T => Future[T] = funs.reduceLeft((f, g) => doInOrder(f, g))

      val waitDur = Duration(4 * waitFutMsecs, MILLISECONDS)
      println("Await.result(doInOrder(delay((_: String).length), delay((_: Int) * 2))(\"TEST\"), waitDur) = " +
        Await.result(doInOrder(delay((_: String).length), delay((_: Int) * 2))("TEST"), waitDur))
      println("Await.result(chainFutures(delay((_: Int) + 3), delay(100 % (_: Int)), delay((i: Int) => i * i))(10), waitDur) = " +
        Await.result(chainFutures(delay((_: Int) + 3), delay(100 % (_: Int)), delay((i: Int) => i * i))(10), waitDur))
      println("Await.result(chainFutures(Seq(delay((_: Int) + 3), delay(100 % (_: Int)), delay((i: Int) => i * i)): _*)(10), waitDur) = " +
        Await.result(chainFutures(Seq(delay((_: Int) + 3), delay(100 % (_: Int)), delay((i: Int) => i * i)): _*)(10), waitDur))
    }

    exercise(4) {
      def doTogether[T, U1, U2](f1: T => Future[U1], f2: T => Future[U2]): T => Future[(U1, U2)] = input =>
        f1(input) zip f2(input)

      val waitDur = Duration(1.5 * waitFutMsecs, MILLISECONDS)
      println("Await.result(doTogether(delay((_: String).length), delay((_: String) * 2))(\"TEST\"), waitDur) = " +
        Await.result(doTogether(delay((_: String).length), delay((_: String) * 2))("TEST"), waitDur))
    }

    exercise(5) {
      def parallelFutures(futures: Future[Any]*): Future[Seq[Any]] = Future.sequence(futures)

      val waitDur = Duration(1.5 * waitFutMsecs, MILLISECONDS)
      println("Await.result(parallelFutures(delayed(1), delayed(2), delayed(3)), waitDur) = " +
         Await.result(parallelFutures(delayed(1), delayed(2), delayed(3)), waitDur))
      println("Await.result(parallelFutures(Seq(delayed(1), delayed(2), delayed(3)): _*), waitDur) = " +
        Await.result(parallelFutures(Seq(delayed(1), delayed(2), delayed(3)): _*), waitDur))
    }

    exercise(6) {
      def repeat[T](action: () => T, until: T => Boolean): Future[T] = {

        @tailrec def cycle(): T = {
          val res = action()
          if (until(res)) res else cycle()
        }

        Future[T] { cycle() }
      }

      def inputPassword(): String = { print("enter password: > "); scala.io.StdIn.readLine() }

      def passwordIsCorrect(pwd: String): Boolean = pwd == "secret"

      println(s"You have found the correct password '${Await.result(repeat(inputPassword, passwordIsCorrect), Duration.Inf)}'.")
    }

    exercise(7) {

      def countPrimes(upToN: BigInt, certainty: Int): Int = {
        val nCores = Runtime.getRuntime.availableProcessors
        val (quotient, remainder) = upToN /% nCores
        val intsPerCore: BigInt = if (remainder == 0) quotient else quotient + 1
        var futureList = List[Future[Int]]()
        for (c <- 0 until nCores) {
          val base = intsPerCore * c
          val range = (base + 1) to ((base + intsPerCore) min upToN)
          futureList = Future {
            val count = range.count(n => n.isProbablePrime(certainty))
            println(s"$threadAndTime: $range => count = $count")
            count
          } :: futureList
        }
        val future = Future.sequence(futureList)
        Await.result(future, Duration.Inf).sum
      }

      val n = 103
      val certainty = 10
      println(s"Among the first $n integers, ${countPrimes(n, certainty)} are probably primes with certainty $certainty.")
    }

    exercise(8, 9) {
      analyzeUrlServers(collectServerStats)
    }

    exercise(10) {
      analyzeUrlServers(collectServerStatsUsingTrieMap)
    }

    exercise(11) {
      Chapter17Exercise11.main(Array.empty)
    }

    exercise(12) {
      Chapter17Exercise12.main(Array.empty)
    }

    exercise(13) {
      Chapter17Exercise13.main(Array.empty)
    }

  }

}
