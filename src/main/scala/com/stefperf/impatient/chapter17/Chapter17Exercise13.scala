package com.stefperf.impatient.chapter17

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.Success

object Chapter17Exercise13 extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  def isPalindromic(bi: BigInt): Boolean = {
    val str = bi.toString
    val len = str.length
    (0 until (len / 2)).map{ indexFromLeft => (indexFromLeft, len - 1 - indexFromLeft) }.forall{
      case (indexFromLeft, indexFromRight) => str(indexFromLeft) == str(indexFromRight)
    }
  }

  // find a BigInt satisfying the predicate as fast as possible, using all cores, withing the two extremes (included)
  def searchInRange(lower: BigInt, upper: BigInt, predicate: BigInt => Boolean): Future[BigInt] = {
    assert(lower <= upper)
    val nNumbers = upper - lower + 1
    val nCores = Runtime.getRuntime.availableProcessors
    val nNumbersPerCore = if (nNumbers % nCores == 0) nNumbers / nCores else nNumbers / nCores + 1
    val subranges = (lower to upper by nNumbersPerCore).map{ startNum => (startNum, (startNum + nNumbersPerCore - 1) min upper)}
    val found = Promise[BigInt]()
    val workers = for (((subLower, subUpper), coreId) <- subranges.zipWithIndex) yield Future {
      for (bi <- subLower to subUpper if !found.isCompleted) {
        println(s"Core # $coreId is checking $bi...")
        if (predicate(bi)) found.trySuccess(bi)
      }
    }
    Future {  // ensure the promise completes in failure if no number is found
      Await.result(Future.sequence(workers), Duration.Inf)
      if (!found.isCompleted) found.tryFailure(new IllegalArgumentException())
    }
    found.future
  }

  def searchPalindromicInRange(lower: BigInt, upper: BigInt): Unit = {
    println(s"Looking for a palindromic number in [$lower, $upper]...")
    def searchTillEnd = searchInRange(lower, upper, isPalindromic).andThen {
      case _ => Thread.sleep(1000)  // to allow time for all threads to finish printing
    }.andThen {
      case Success(palindromic) => println(s"The first palindromic found in [$lower, $upper] was $palindromic.")
    }.recover {
      case ex: IllegalArgumentException => println(s"No palindromic number could be found in [$lower, $upper].")
    }
    Await.ready(searchTillEnd, Duration.Inf)
    println()
  }

  searchPalindromicInRange(102, 110)
  searchPalindromicInRange(1234, 2345)
  Thread.sleep(1000)  // to make sure that all other threads finish
}
