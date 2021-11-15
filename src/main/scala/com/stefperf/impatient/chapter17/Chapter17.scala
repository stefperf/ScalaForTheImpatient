package com.stefperf.impatient.chapter17

import com.stefperf.impatient._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


object Chapter17 extends Chapter(17, "Futures") {
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
        val cores = Runtime.getRuntime.availableProcessors
        val (quotient, remainder) = upToN /% cores
        val intsPerCore: BigInt = if (remainder == 0) quotient else quotient + 1
        var futureList = List[Future[Int]]()
        for (c <- 0 until cores) {
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
      import java.net.URL
      import scala.io.Source

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
            println(s"It was not possible to complete the operation due to this error:\n$ex")
            None
        }
        Await.result(futureUrlList, maxDuration)
      }

      def getServer(url: URL): Option[String] =
        Try{ Option(url.openConnection().getHeaderField("Server")) }.fold[Option[String]](_ => None, os => os)

      val maxSecondsOp1 = 60
      println(s"In the next $maxSecondsOp1 seconds, we will try to read and process a URL from the user.")
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
          val maxSecondsOp2 = 30
          println(s"In the next $maxSecondsOp2 seconds, we will try to compile a list of all server headers at those links.")
          val futureServers = Future.sequence(
            uniqueUrls.map(
              url => Future { getServer(url) }.recover{ case _ => None }
            )
          )
          Await.ready(futureServers, Duration(maxSecondsOp2, SECONDS))
          val Some(result) = futureServers.value
          result match {
            case Failure(exception) => println(s"The operation failed with error:\n$exception")
            case Success(seq) =>
              val servers = seq.flatten.distinct.sorted
              if (servers.isEmpty)
                println("No server headers were found.")
                else {
                  println("-- List of server headers found:")
                for (server <- servers) println(s"- $server")
                  println("-- End of list.")
                }
          }
      }
    }

    println("WORK IN PROGRESS")
    }

}
