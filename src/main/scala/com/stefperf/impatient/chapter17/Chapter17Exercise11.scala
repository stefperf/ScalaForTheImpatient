package com.stefperf.impatient.chapter17

import java.util.concurrent.Executors
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

object Chapter17Exercise11 extends App {
  def elapsedSecs(startMsecs: Long): Double = (System.currentTimeMillis - startMsecs) / 1000

  val nCores = Runtime.getRuntime.availableProcessors
  val nSeconds = 1.5
  val nsThreads = List(4, 20)

  for (nThreads <- nsThreads) {
    import scala.concurrent.ExecutionContext.Implicits.global
    println(s"$nThreads threads each lasting $nSeconds seconds in the global thread pool:")
    val startMsecs = System.currentTimeMillis
    val futureSeq = Future.sequence(
      (1 to nThreads).map(i => Future { Thread.sleep((nSeconds * 1000).toLong); println(f"Thread # $i finished @ ${elapsedSecs(startMsecs)}%.3f") })
    )
    Await.result(futureSeq, Duration(1.1 * nSeconds * (nThreads.toDouble / nCores).ceil, SECONDS))
    println()
  }
  println(s"The bottleneck of having only $nCores cores throttles the execution of these threads.")
  println()
  println()
  for (nThreads <- nsThreads) {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
    println(s"$nThreads threads each lasting $nSeconds seconds in a dedicated thread pool:")
    val startMsecs = System.currentTimeMillis
    val futureSeq = Future.sequence(
      (1 to nThreads).map(i => Future { Thread.sleep((nSeconds * 1000).toLong); println(f"Thread # $i finished @ ${elapsedSecs(startMsecs)}%.3f") })
    )
    Await.result(futureSeq, Duration(1.1 * nSeconds, SECONDS))
    println()
  }
  println(s"These threads can all be executed at the same time.")

}
