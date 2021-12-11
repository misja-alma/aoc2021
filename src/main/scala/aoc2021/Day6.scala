package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

object Day6Part1 extends IOApp {

  def shiftLanternFishes(fishes: Map[Int, Int]): Map[Int, Int] = {
    (0 to 8).map {
      case 8 => (8, fishes.getOrElse(0, 0))
      case 6 => (6, fishes.getOrElse(7, 0) + fishes.getOrElse(0, 0))
      case x => (x, fishes.getOrElse(x + 1, 0))
    }.toMap
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day6.txt")
      nrs = sc.nextLine().split(",").map(_.toInt)
      initialMap = nrs.groupBy(identity).view.mapValues(_.length).toMap
      generation80 = Iterator.iterate(initialMap)(shiftLanternFishes).drop(80).next()
      solution = generation80.values.sum
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day6Part2 extends IOApp {

  def shiftLanternFishes(fishes: Map[Int, Long]): Map[Int, Long] = {
    (0 to 8).map {
      case 8 => (8, fishes.getOrElse(0, 0L))
      case 6 => (6, fishes.getOrElse(7, 0L) + fishes.getOrElse(0, 0L))
      case x => (x, fishes.getOrElse(x + 1, 0L))
    }.toMap
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day6.txt")
      nrs = sc.nextLine().split(",").map(_.toInt)
      initialMap = nrs.groupBy(identity).view.mapValues(_.length.toLong).toMap
      generation256 = Iterator.iterate(initialMap)(shiftLanternFishes).drop(256).next()
      solution = generation256.values.sum
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

