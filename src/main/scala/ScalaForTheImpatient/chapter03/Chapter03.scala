package ScalaForTheImpatient.chapter03

import ScalaForTheImpatient._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

object Chapter03 extends Chapter(3, "Working with Arrays", {
  exercise(1) {
    def randomIntArray(n: Int) = (for (_ <- 0 until n) yield scala.util.Random.nextInt(n)).toArray

    println(seq2line(randomIntArray(3)))
    println(seq2line(randomIntArray(8)))
  }

  exercise(2) {
    val a = Array(1, 2, 3, 4, 5)

    def swapPairwise[T: ClassTag](a: Array[T]): Unit = {
      for (i <- 0 until (a.length - 1) by 2) {
        val temp = a(i)
        a(i) = a(i + 1)
        a(i + 1) = temp
      }
    }

    swapPairwise(a)
    println(seq2line(a))
  }

  exercise(3) {
    val a = Array(1, 2, 3, 4, 5)

    def swapPairwiseV1[T: ClassTag](a: Array[T]): Array[T] =
      (for (i <- a.indices by 2; j <- 1 to 0 by -1; k = i + j if k < a.length) yield a(k)).toArray

    println(seq2line(swapPairwiseV1(a)))

    def swapPairwiseV2[T: ClassTag](a: Array[T]): Array[T] =
      (for (pairOrLast <- a.sliding(2, 2)) yield pairOrLast.reverse).flatten.toArray

    println(seq2line(swapPairwiseV2(a)))
  }

  exercise(4) {
    def posIntsFirst(arr: Array[Int]): Array[Int] = {
      val posEls, nonPosEls = ArrayBuffer[Int]()
      for (el <- arr) if (el > 0) posEls += el else nonPosEls += el
      (posEls ++ nonPosEls).toArray
    }

    val arr = Array(4, 0, -2, -7, 5, -1, 4, 2, -1, -7)
    println(seq2line(posIntsFirst(arr)))
  }

  exercise(5) {
    val arr = Array(0, 1, 10, 100.0)

    def arrAvg(arr: Array[Double]): Double = arr.sum / arr.length

    println(arrAvg(arr))
  }

  exercise(6) {
    val a = Array(1, 2, 3, 4, 5)

    def reverseInPlace[T](a: mutable.IndexedSeq[T]): Unit =
      for (i <- 0 until a.length / 2; j = a.length - 1 - i) {
        val temp = a(i); a(i) = a(j); a(j) = temp
      }

    println(seq2line(a.reverse))
    reverseInPlace(a)
    println(seq2line(a))
    val ab = ArrayBuffer(1, 2, 3, 4, 5)
    println(seq2line(ab.reverse))
    reverseInPlace(ab)
    println(seq2line(ab))
  }

  exercise(7) {
    val length = 20
    val nMax = 5
    val arrayWithDuplicates = (1 to length).map(_ => Random.nextInt(nMax + 1)).toArray
    printseqline(arrayWithDuplicates)
    printseqline(arrayWithDuplicates.distinct)
  }

  exercise(8) {
    def removeAllButFirstNegatives(ab: ArrayBuffer[Int]): Unit = {
      val indicesToRemove = (for ((el, i) <- ab.zipWithIndex if el < 0) yield i).drop(1)
      for (i <- indicesToRemove.reverse) ab.remove(i)
    }

    val ab = ArrayBuffer(4, 2, -3, -6, 6, 7, -5)
    printseqline(ab)
    removeAllButFirstNegatives(ab)
    printseqline(ab)
  }

  exercise(9) {
    def removeAllButFirstNegatives(arrbuf: ArrayBuffer[Int]): Unit = {
      var firstNegativeIndex: Option[Int] = None
      val indicesOfElementsToMove = ArrayBuffer[Int]()
      for ((el, i) <- arrbuf.zipWithIndex) {
        if (el < 0) {
          if (firstNegativeIndex.isEmpty) {
            firstNegativeIndex = Some(i)
            indicesOfElementsToMove += i
          }
        }
        else {
          if (firstNegativeIndex.isDefined) indicesOfElementsToMove += i
        }
      }
      if (firstNegativeIndex.isDefined) {
        val nrInitialPositiveElements = firstNegativeIndex.get
        var newIndex = nrInitialPositiveElements
        for (oldIndex <- indicesOfElementsToMove) {
          arrbuf(newIndex) = arrbuf(oldIndex)
          newIndex += 1
        }
        val newLength = nrInitialPositiveElements + indicesOfElementsToMove.length
        arrbuf.dropRight(arrbuf.length - newLength)
      }
    }

    def demotest(ab: ArrayBuffer[Int]): Unit = {
      printseqline(ab, end = " => ")
      removeAllButFirstNegatives(ab)
      printseqline(ab)
    }

    demotest(ArrayBuffer(4, 2, -3, -6, 6, 7, -5))
    demotest(ArrayBuffer())
    demotest(ArrayBuffer(-1, -2, -3, -4))
    demotest(ArrayBuffer(1, 2, 3, 4))
    demotest(ArrayBuffer(1, 2, 3, -4, -5))
    demotest(ArrayBuffer(-1, -2, -3, 4, 5))
  }

  exercise(10) {
    import java.util.TimeZone
    val prefixToStrip = "America/"
    val prefixes = Array(prefixToStrip, "US/")
    val timeZonesInAmerica = (
      for (timeZoneId <- TimeZone.getAvailableIDs; prefix <- prefixes if timeZoneId.startsWith(prefix))
        yield if (timeZoneId.startsWith(prefixToStrip)) timeZoneId.drop(prefixToStrip.length) else timeZoneId
      ).sorted
    for (timeZoneName <- timeZonesInAmerica) println(timeZoneName)
  }

  exercise(11) {
    import java.awt.datatransfer._
    import scala.collection.JavaConversions.mapAsScalaMap
    val flavors = SystemFlavorMap.getDefaultFlavorMap.asInstanceOf[SystemFlavorMap]
    val nativesForImageFlavor: scala.collection.Map[java.awt.datatransfer.DataFlavor,String] =
      flavors.getNativesForFlavors(Array(DataFlavor.imageFlavor))
    println("val nativesForImageFlavor = Map[DataFlavor, String](")
    println(map2str(nativesForImageFlavor))
    println(")")
  }
})
