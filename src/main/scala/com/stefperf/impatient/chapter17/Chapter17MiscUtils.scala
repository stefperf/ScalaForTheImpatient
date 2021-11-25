package com.stefperf.impatient.chapter17

import java.net.URL
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Chapter17MiscUtils {

  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  @tailrec def inputUrl(): URL = {
    print("enter URL: > ")
    val input = scala.io.StdIn.readLine()
    val urlTry = Try{ new URL(input) }
    urlTry match {
      case Success(url) => url
      case _ =>
        println(s"'$input' is not a valid URL. Please try again.")
        inputUrl()
    }
  }

  def readWebPage(url: URL): String = {
    try {
      Source.fromURL(url).mkString
    }
    catch {
      case ex =>  // intentionally catching all throwables
        println(s"It was not possible to read the webpage at address '$url' due to this error:\n$ex")
        ""
    }
  }

  def extractAbsoluteHyperlinks(html: String): List[URL] = {
    val absoluteLinkPattern = """<.*? href=["'](http.*?)["'].*?>""".r("href")
    (for (anchor <- absoluteLinkPattern.findAllMatchIn(html); link = anchor.group("href").toLowerCase)
      yield Try{ new URL(link)}.toOption
      ).toList.flatten
  }

  def getValidAbsoluteHyperlinks(maxDuration: Duration): Option[List[URL]] = {
    val futureUrlList = Future{ inputUrl() }.flatMap(
      url => Future{ readWebPage(url) }).flatMap(
      html => Future{ Option(extractAbsoluteHyperlinks(html)) }).recover{
      case ex =>
        println(s"It was not possible to complete the operation in the given time due to this error:\n$ex")
        None
    }
    Await.result(futureUrlList, maxDuration)
  }

  def getServer(url: URL): Option[String] =
    Try { Option( url.openConnection().getHeaderField("Server") ) }.fold[Option[String]](_ => None, os => os)

  def canConnectToUrl(url: URL, maxSeconds: Int): Boolean = Await.result(
      Future { url.openConnection() }.map{_ => true}.recover[Boolean]{ case _: Throwable => false },
      Duration(maxSeconds, SECONDS)
    )

  // read a URL from the user, find all unique URLs linked from that web page, then finally
  // display the occurrence counts of servers at those links returned by function collectServerStats
  def analyzeUrlServers(collectServerStats: (List[URL], Int) => Map[String, Int]): Unit = {
    val (maxSecondsOp1, maxSecondsOp2) = (60, 30)
    println(s"In the next $maxSecondsOp1 nSeconds, we will try to read and process a URL from the user.")
    val result = getValidAbsoluteHyperlinks(Duration(maxSecondsOp1, SECONDS))
    result match {
      case None =>
      case Some(Nil) => println("No valid absolute hyperlinks were found in the given webpage.")
      case Some(urls) =>
        val uniqueUrls = urls.distinct
        println("-- List of valid absolute hyperlinks found in the given webpage:")
        for (url <- uniqueUrls.sortBy(_.toString)) {
          println(s"- $url")
        }
        println("-- End of list.")
        println()
        println(s"In the next $maxSecondsOp2 nSeconds, we will try to compile a list of all server headers at those links.")
        val serversToCounts = collectServerStats(uniqueUrls, maxSecondsOp2)
        if (serversToCounts.isEmpty)
          println("No server headers were found.")
        else {
          val serversByDecreasingCounts = mutable.LinkedHashMap[String, Int]() ++
            serversToCounts.toSeq.sortBy { case (server, count) => (-count, server) }
          println("-- List of server headers found:")
          for ((server, count) <- serversByDecreasingCounts) println(f"- $count occurrences of $server")
          println("-- End of list.")
        }

    }
  }

  def collectServerStats(uniqueUrls: List[URL], maxSeconds: Int): Map[String, Int] = {
    val futureServers = Future.sequence(
      uniqueUrls.map { url =>
        Future { getServer(url) }.recover { case _ => None }
      }
    )
    Await.ready(futureServers, Duration(maxSeconds, SECONDS))
    val Some(result) = futureServers.value
    result match {
      case Failure(exception) => println(s"The operation failed with error:\n$exception"); Map[String, Int]()
      case Success(optionalServers) => optionalServers.flatten.groupBy(identity).mapValues(_.length)
    }
  }

  def collectServerStatsUsingTrieMap(uniqueUrls: List[URL], maxSeconds: Int): Map[String, Int] = {
    val serverCounts = TrieMap[String, Int]()

    def incrementCount(server: String): Unit = synchronized {
      serverCounts.put(server, serverCounts.getOrElse(server, 0) + 1)
    }

    val futureServers = Future.sequence(
      uniqueUrls.map { url =>
        Future { getServer(url).fold({})(incrementCount) }
      }
    )
    Await.ready(futureServers, Duration(maxSeconds, SECONDS))
    serverCounts.toMap
  }

}
