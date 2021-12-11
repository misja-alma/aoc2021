import cats.effect.IO

import java.io.InputStream
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

package object aoc2021 {
  def scannerFromResource(resourcePath: String): IO[Scanner] = {
    val istream: InputStream = getClass.getResourceAsStream(resourcePath)
    IO.pure(new Scanner(istream, "UTF-8"))
  }

  def scannerToLines(sc: Scanner): Seq[String] = {
    val lineReader = sc.useDelimiter("\n")
    val result = ArrayBuffer[String]()
    while (lineReader.hasNext) result.append(lineReader.next())
    result.toSeq
  }

  case class Point(x: Int, y: Int)

  object Point {
    def apply(s: String): Point = {
      val Array(x, y) = s.split(",").map(_.toInt)
      Point(x, y)
    }
  }

  class Grid(grid: Array[Array[Int]]) {
    def value(point: Point): Int = grid(point.y)(point.x)

    def update(point: Point, value: Int): Unit = grid(point.y)(point.x) = value

    def allPoints: Seq[Point] =
      for {
        y <- grid.indices
        x <- grid.head.indices
      } yield Point(x, y)

    def neighbours(point: Point): Seq[Point] = {
      val raw = Seq(Point(point.x-1, point.y), Point(point.x, point.y-1), Point(point.x+1, point.y), Point(point.x, point.y+1))
      raw.filter(p => p.x >= 0 && p.x < grid.head.length && p.y >= 0 && p.y < grid.length)
    }

    def allNeighbours(point: Point): Seq[Point] = {
      val raw = Seq(Point(point.x-1, point.y), Point(point.x-1, point.y-1), Point(point.x - 1, point.y+1),
        Point(point.x, point.y-1),  Point(point.x, point.y+1),
        Point(point.x+1, point.y), Point(point.x+1, point.y-1), Point(point.x+1, point.y+1))
      raw.filter(p => p.x >= 0 && p.x < grid.head.length && p.y >= 0 && p.y < grid.length)
    }

    def forall(pred: Int => Boolean): Boolean = {
      grid.forall(_.forall(pred))
    }
  }
}
