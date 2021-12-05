package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

case class Point(x: Int, y: Int)

object Point {
  def apply(s: String): Point = {
    val Array(x, y) = s.split(",").map(_.toInt)
    Point(x, y)
  }
}

object Day5Part1 extends IOApp {

  def parseLine(line: String): (Point, Point) = {
    val Array(from, to) = line.split(" -> ")
    (Point(from), Point(to))
  }

  def drawLine(line: (Point, Point), map: Map[Point, Int]): Map[Point, Int] = {
    val (from, to) = line
    // if vert or hor, draw line as points and add to map
    val points = if (from.x == to.x) {
      (from.y to to.y by (to.y - from.y) / Math.abs(to.y - from.y)).map(Point(from.x, _))
    } else
      if (from.y == to.y) {
        (from.x to to.x by (to.x - from.x) / Math.abs(to.x - from.x)).map(Point(_, from.y))
      } else Seq()
      
    points.foldLeft(map) { case (mp, pt) =>
      val existing = mp.getOrElse(pt, 0)
      val newTuple = (pt, existing + 1)
      mp + newTuple  // For some reason the variable assignment to newTuple is needed
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day5.txt")
      lines = scannerToLines(sc).map(parseLine)
      points = lines.foldLeft(Map[Point, Int]()){ case (map , line) => drawLine(line, map) }
      solution = points.values.count(_ >= 2)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day5Part2 extends IOApp {

  def parseLine(line: String): (Point, Point) = {
    val Array(from, to) = line.split(" -> ")
    (Point(from), Point(to))
  }

  def drawLine(line: (Point, Point), map: Map[Point, Int]): Map[Point, Int] = {
    val (from, to) = line
    // if vert or hor, draw line as points and add to map
    val points = if (from.x == to.x) {
      (from.y to to.y by (to.y - from.y) / Math.abs(to.y - from.y)).map(Point(from.x, _))
    } else
      if (from.y == to.y) {
        (from.x to to.x by (to.x - from.x) / Math.abs(to.x - from.x)).map(Point(_, from.y))
      } else {
        // diagonal
        val dx = (to.x - from.x) / Math.abs(to.x - from.x)
        val dy = (to.y - from.y) / Math.abs(to.y - from.y)
        val xs = from.x to to.x by dx
        val ys = from.y to to.y by dy
        xs.zip(ys).map { case (x, y) => Point(x, y) }
      }

    points.foldLeft(map) { case (mp, pt) =>
      val existing = mp.getOrElse(pt, 0)
      val newTuple = (pt, existing + 1)
      mp + newTuple  // For some reason the variable assignment to newTuple is needed
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day5.txt")
      lines = scannerToLines(sc).map(parseLine)
      points = lines.foldLeft(Map[Point, Int]()){ case (map , line) => drawLine(line, map) }
      solution = points.values.count(_ >= 2)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

