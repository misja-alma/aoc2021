package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day9Part1 extends IOApp {
  
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day9.txt")
      lines = scannerToLines(sc)
      grid = new Grid(lines.map(_.map(_.asDigit).toArray).toArray)
      lowPoints = grid.allPoints.filter { p =>
        val nb = grid.neighbours(p)
        nb.forall { np => grid.value(p) < grid.value(np) }
      }
      solution = lowPoints.map(p => grid.value(p) + 1).sum
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day9Part2 extends IOApp {

  def basin(grid: Grid)(point: Point): Set[Point] = {
    // bfs; add neighbours without 9's and without visited points
    val visited = mutable.Set(point)  // start at point
    val next = mutable.Queue().addAll(grid.neighbours(point).filterNot(p => grid.value(p) == 9))
    while (next.nonEmpty) {
      val np = next.dequeue()
      visited += np
      val newPoints = grid.neighbours(np).filterNot(p => grid.value(p) == 9).filterNot(visited.contains)
      next.addAll(newPoints)
    }
    visited.toSet
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day9.txt")
      lines = scannerToLines(sc)
      grid = new Grid(lines.map(_.map(_.asDigit).toArray).toArray)
      lowPoints = grid.allPoints.filter { p =>
        val nb = grid.neighbours(p)
        nb.forall { np => grid.value(p) < grid.value(np) }
      }
      basins = lowPoints.map(basin(grid)).map(_.size).sorted.reverse
      solution = basins.take(3).product
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}