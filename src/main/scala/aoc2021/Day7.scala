package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

object Day7Part1 extends IOApp {

  // TODO
  // 1. use iterator?
  // 2. binary search must be faster. But upperbound can't be max because that's O(n)
  //    otoh linear search can hold a running sum. Just needs to know how many points smaller and how many larger,
  //    and needs sorted array
  def findNearestPoint(ints: Array[Int]): Int = {
    var pos = ints.min
    var best = ints.sum - pos * ints.length
    var found = false
    while (!found) {
      pos = pos + 1
      val newDist = ints.map(n => Math.abs(pos - n)).sum
      if (newDist > best) {
        found = true
      } else {
        best = newDist
      }
    }
    pos - 1
  }

  def findNearestPoint2(ints: Array[Int]): Int = {
    val sorted = ints.sorted
    val pos = 0
    val posInArray = 0
    val best = ints.sum
    val nrSmaller = 0
    val found = false
    val result = Iterator.iterate((pos, posInArray, best, nrSmaller, found)) { case ((pos, posInArray, best, nrSmaller, found)) =>
       // TODO skip nrs in array equal to post. the # skipped should be added to nrSmaller
       // increase pos
       // calc new best by adding nrSmaller and subtracting nrBigger
      // if new best < oldBest then found
      (pos, posInArray, best, nrSmaller, found)
    }.dropWhile(!_._5).next()

    result._1
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day7.txt")
      nrs = sc.nextLine().split(",").map(_.toInt)
      nearest = findNearestPoint(nrs)
      distance = nrs.map { nr => Math.abs(nearest - nr) }.sum
      _ <- IO.delay(println("Solution: " + distance))     // 344138
    } yield ExitCode.Success
  }
}

object Day7Part2 extends IOApp {

  def increasingDistance(from: Int)(to: Int): Int = {
    val dist = Math.abs(to - from)
    (1 + dist * (dist + 1))/2
  }

  def findNearestPoint(ints: Array[Int]): Int = {
    var pos = ints.min
    var best = ints.map(increasingDistance(pos)).sum
    var found = false
    while (!found) {
      pos = pos + 1
      val newDist = ints.map(increasingDistance(pos)).sum
      if (newDist > best) {
        found = true
      } else {
        best = newDist
      }
    }
    pos - 1
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day7.txt")
      nrs = sc.nextLine().split(",").map(_.toInt)
      nearest = findNearestPoint(nrs)
      distance = nrs.map(increasingDistance(nearest)).sum
      _ <- IO.delay(println("Solution: " + distance))
    } yield ExitCode.Success
  }
}

