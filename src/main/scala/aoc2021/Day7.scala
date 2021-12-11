package aoc2021

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Day7Part1 extends IOApp {

  // TODO
  // 1. use iterator?
  // 2. binary search must be faster. But upperbound can't be max because that's O(n)
  //    otoh linear search can hold a running sum. Just needs to know how many points smaller and how many larger,
  //    and needs sorted array
  def findNearestPoint(ps: Array[Int]): Int = {
    var pos = ps.min
    var best = ps.sum - pos * ps.length
    var found = false
    while (!found) {
      pos = pos + 1
      val newDist = ps.map(n => Math.abs(pos - n)).sum
      if (newDist > best) {
        found = true
      } else {
        best = newDist
      }
    }
    pos - 1
  }

  def findNearestPointPure(ps: Array[Int]): Int = {
    val pos = ps.min
    val best = ps.sum - pos * ps.length

    val tries = iterate((pos, best, false)) { case (pos, best, _) =>
      val newPos = pos + 1
      val newDist = ps.map(n => Math.abs(newPos - n)).sum
      if (newDist > best) {
        (pos, best, true)
      } else {
        (newPos, newDist, false)
      }
    }

    val Some((solution, _, _)) = tries.find(_._3)
    solution
  }

  def findNearestPointSmart(ps: Array[Int]): Int = {
    // keep a running sum of smaller elements, so we don't have to recompute all the distances every time
    val sorted = ps.sorted
    var pos = 0
    var indexInArray = 0
    var best = ps.sum
    var nrSmaller = 0
    var found = false
    while (!found) {
      val nrBigger = ps.length - nrSmaller
      while (sorted(indexInArray) == pos) {
        nrSmaller += 1
        indexInArray += 1
      }
      pos += 1
      val newBest = best + nrSmaller - nrBigger
      if (newBest > best) {
        found = true
      } else {
        best = newBest
      }
    }

    pos - 1
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day7.txt")
      nrs = sc.nextLine().split(",").map(_.toInt)
      nearest = findNearestPointSmart(nrs)
      distance = nrs.map { nr => Math.abs(nearest - nr) }.sum
      _ <- IO.delay(println("Solution: " + distance)) // 344138
    } yield ExitCode.Success
  }
}

object Day7Part2 extends IOApp {

  def increasingDistance(from: Int)(to: Int): Int = {
    val dist = Math.abs(to - from)
    (1 + dist * (dist + 1)) / 2
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

