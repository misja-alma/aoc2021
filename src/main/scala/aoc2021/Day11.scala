package aoc2021

import aoc2021.Day11.updateAndCountFlashes
import cats.effect.{ExitCode, IO, IOApp}

object Day11 {

  def updateAndCountFlashes(grid: Grid, totalFlashes: Int): (Grid, Int) = {
    var newFlashes = 0
    var flashes = grid.allPoints.flatMap { p =>
      val current = grid.value(p)
      if (current == 9) {
        grid.update(p, 0)
        Some(p)
      } else {
        grid.update(p, current + 1)
        None
      }
    }
    newFlashes += flashes.size

    while (flashes.nonEmpty) {
      val newerFlashes = flashes.flatMap { f =>
        grid.allNeighbours(f).flatMap { nb =>
          val current = grid.value(nb)
          if (current == 9) {
            grid.update(nb, 0)
            Some(nb)
          } else {
            if (current != 0) grid.update(nb, current + 1)
            None
          }
        }
      }
      flashes = newerFlashes
      newFlashes += flashes.size
    }

    (grid, totalFlashes + newFlashes)
  }
}

object Day11Part1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day11.txt")
      lines = scannerToLines(sc).map(_.trim)
      grid = new Grid(lines.map(_.map(_.asDigit).toArray).toArray)
      generation100 = Iterator
        .iterate((grid, 0)){ case (grid, nrFlashes) => updateAndCountFlashes(grid, nrFlashes) }
        .drop(100)
        .next
      _ <- IO.delay(println("Solution: " + generation100._2))
    } yield ExitCode.Success
  }
}

object Day11Part2 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day11.txt")
      lines = scannerToLines(sc).map(_.trim)
      grid = new Grid(lines.map(_.map(_.asDigit).toArray).toArray)
      (_, generation) = Iterator
        .iterate((grid, 0)){ case (grid, nrFlashes) => updateAndCountFlashes(grid, nrFlashes) }
        .zipWithIndex
        .find { case ((grid, _), _) => grid.forall(_ == 0) }
        .get
      _ <- IO.delay(println("Solution: " + generation))
    } yield ExitCode.Success
  }
}