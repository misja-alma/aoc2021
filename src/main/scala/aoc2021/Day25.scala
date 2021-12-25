package aoc2021

import aoc2021.Grid.printGrid
import cats.effect.{ExitCode, IO, IOApp}

object Day25Part1 extends IOApp {

  var count = 1
  def nextMove(grid: Grid[Char]): Grid[Char] = {
    println ("Generation: " + count)
    count = count + 1
    printGrid(grid)
    println()

    val newGrid = Grid.withDimensions[Char](grid.width, grid.height, '.')
    // first east, then south
    // east: from left to right, south: from top to bottom
    (0 until grid.width).foreach { x =>
      (0 until grid.height).foreach { y =>
        val pt = Point(x, y)
        if (grid.value(pt) == '>') {
          val nextPt = if (x == grid.width - 1) Point(0, y) else Point(x + 1, y)
          if (grid.value(nextPt) == '.') {
            newGrid.update(nextPt, '>')
          } else {
            newGrid.update(pt, '>')
          }
        }
      }
    }

    (0 until grid.width).foreach { x =>
      (0 until grid.height).foreach { y =>
        val pt = Point(x, y)
        if (grid.value(pt) == 'v') {
          val nextPt = if (y == grid.height - 1) Point(x, 0) else Point(x, y + 1)
          if (grid.value(nextPt) != 'v' && newGrid.value(nextPt) != '>') {
            newGrid.update(nextPt, 'v')
          } else {
            newGrid.update(pt, 'v')
          }
        }
      }
    }

    newGrid
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day25.txt")
      lines = scannerToLines(sc).map(_.trim)
      grid = new Grid[Char](lines.map(_.toArray).toArray)
      Some((_, iteration)) = LazyList
          .iterate(grid)(nextMove)
          .sliding(2, 1)
          .zipWithIndex
          .find { case (Seq(g1, g2), _) => g1 == g2 }
      _ <- IO.delay(println("Solution: " + (iteration + 1)))
    } yield ExitCode.Success
  }
}
