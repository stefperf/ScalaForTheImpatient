package com.stefperf.impatient.chapter17

import com.stefperf.impatient.chapter17.Chapter17MiscUtils.{canConnectToUrl, extractAbsoluteHyperlinks, readWebPage}

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}


object Chapter17Exercise12 extends App {
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def linksAreReachableWithinTime(url: URL, maxSeconds: Int): Seq[Future[(URL, Boolean)]] = {
    val urls = extractAbsoluteHyperlinks(readWebPage(url))
    val urlsAreReachable = urls.map{ url => Promise[(URL, Boolean)]() }
    urls.zipWithIndex.foreach { case (url, i) =>
      Future { val urlIsReachable = (url, canConnectToUrl(url, maxSeconds)); urlsAreReachable(i).success(urlIsReachable) }
    }
    urlsAreReachable.map(_.future)
  }

  val url = new URL("https://horstmann.com")
  val maxSeconds = 2
  println(s"Checking whether all links included in webpage '$url' are reachable within $maxSeconds seconds...")
  val futureReachabilities = Future.sequence(linksAreReachableWithinTime(url, maxSeconds))
  futureReachabilities.onComplete {
    case Success(reachabilities) =>
      for ((url, reachable) <- reachabilities)
        println(s"- $url could ${if (reachable) "" else "not"}be reached")
    case Failure(ex) =>
      println(s"There was this problem:\n$ex")
  }
  Thread.sleep(maxSeconds * 1500)
  println("Returning a sequence of promises would give way too much power to the caller of this function.")
}
