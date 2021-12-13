package aoc2021

import aoc2021.Day13.{fold, parseFold}
import cats.effect.{ExitCode, IO, IOApp}

object Day13 {
  def parseFold(line: String): Either[Int, Int] =
    line match {
      case s"fold along x=$value" => Left(value.toInt)
      case s"fold along y=$value" => Right(value.toInt)
      case _ => sys.error("Cant' parse line: " + line)
    }

  // assume that the folded part is never larger than the part folded over?
  // maybe fold in place, just ignore foldOvers < 0 and clear folded cells
  def fold(foldLine: Either[Int, Int], grid: Grid[Boolean]): Grid[Boolean] = foldLine match {
    case Left(x) =>
      (0 until grid.height).foreach { yf =>
        (0 to x).foreach { xf =>
          if (x + x - xf < grid.width) {
            grid.update(Point(xf, yf), grid.value(Point(xf, yf)) || grid.value(Point(x + x - xf, yf)))
            grid.update(Point(x + x - xf, yf), false)
          }
        }
      }
      grid
    case Right(y) =>
      (0 until grid.width).foreach { xf =>
        (0 to y).foreach { yf =>
          if (y + y - yf < grid.height) {
            grid.update(Point(xf, yf), grid.value(Point(xf, yf)) || grid.value(Point(xf, y + y - yf)))
            grid.update(Point(xf, y + y - yf), false)
          }
        }
      }
      grid
  }
}

object Day13Part1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day13.txt")
      lines = scannerToLines(sc).map(_.trim)
      pointLines = lines.takeWhile(_.nonEmpty)
      foldLines = lines.dropWhile(_.nonEmpty).tail
      points = pointLines.map(Point.apply)
      grid = Grid.fromPoints(points)
      folds = foldLines.map(parseFold)
      folded = fold(folds.head, grid)
      solution = folded.count(_ == true)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day13Part2 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day13.txt")
      lines = scannerToLines(sc).map(_.trim)
      pointLines = lines.takeWhile(_.nonEmpty)
      foldLines = lines.dropWhile(_.nonEmpty).tail
      points = pointLines.map(Point.apply)
      grid = Grid.fromPoints(points)
      folds = foldLines.map(parseFold)
      _ = folds.foreach { f => fold(f, grid) }
      _ <- IO.delay(Grid.printBooleanGrid(grid))
    } yield ExitCode.Success
  }
}
