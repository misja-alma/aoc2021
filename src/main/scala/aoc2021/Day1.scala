package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

object Day1Part1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day1.txt")
      lines = scannerToLines(sc).map(_.toInt)
      increases = lines.sliding(2, 1).filter { ar => ar(0) < ar(1) }
      _ <- IO.delay(println("Solution: " + increases.size))
    } yield ExitCode.Success
  }
}

object Day1Part2 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day1.txt")
      lines = scannerToLines(sc).map(_.toInt)
      sums = lines.sliding(3, 1).map { ar => ar(0) + ar(1) + ar(2) }
      increases = sums.sliding(2, 1).filter { ar => ar(0) < ar(1) }
      _ <- IO.delay(println("Solution: " + increases.size))
    } yield ExitCode.Success
  }
}
