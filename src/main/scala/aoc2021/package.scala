import cats.effect.IO

import java.io.InputStream
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

package object aoc2021 {
  def scannerFromResource(resourcePath: String): IO[Scanner] = {
    val istream: InputStream = getClass.getResourceAsStream(resourcePath)
    IO.pure(new Scanner(istream, "UTF-8"))
  }

  def scannerToLines(sc: Scanner): Seq[String] = {
    val result = ArrayBuffer[String]()
    while (sc.hasNext) result.append(sc.nextLine())
    result.toSeq
  }

  case class Point(x: Int, y: Int)

  object Point {
    def apply(s: String): Point =
      s match {
        case s"$x,$y" => Point(x.toInt, y.toInt)
        case _ => sys.error("Cant parse Point: " + s)
    }

    def manhattanDistance(p1: Point, p2: Point): Int = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)
  }

  case class Pos3D(x: Int, y: Int, z: Int) {
    def +(pos2: Pos3D): Pos3D = Pos3D(x + pos2.x, y + pos2.y, z + pos2.z)

    def -(pos2: Pos3D): Pos3D = Pos3D(x - pos2.x, y - pos2.y, z - pos2.z)
  }

  object Grid {
    def withDimensions[T: ClassTag](x: Int, y: Int, initialValue: T): Grid[T] = {
      val ar = (0 until y).map(_ => Array.fill[T](x)(initialValue)).toArray
      new Grid(ar)
    }

    def fromPoints(points: Seq[Point]): Grid[Boolean] = {
      val maxX = points.maxBy(_.x).x
      val maxY = points.maxBy(_.y).y
      val grid = Grid.withDimensions(maxX + 1, maxY + 1, false)
      points.foreach { p => grid.update(p, true) }
      grid
    }

    def printBooleanGrid(grid: Grid[Boolean]): Unit = {
      val allFilled = grid.allPoints.filter { p => grid.value(p)}
      val maxX = allFilled.maxBy(_.x).x
      val maxY = allFilled.maxBy(_.y).y
      (0 to maxY).foreach { row =>
        (0 to maxX).foreach { col =>
          if (grid.value(Point(col, row))) print('#') else print('.')
        }
        println()
      }
    }

    def printGrid(grid: Grid[Char]): Unit = {
      val maxX = grid.width
      val maxY = grid.height
      (0 until maxY).foreach { y =>
        (0 until maxX).foreach { x =>
          print(grid.value(Point(x, y)))
        }
        println()
      }
    }
  }

  /**
   *
   * @param grid main array contains the rows, subArrays the columns.
   * @tparam T
   */
  class Grid[T](grid: Array[Array[T]]) {
    def width: Int = if (grid.isEmpty) 0 else grid.head.length

    def height: Int = grid.length

    def value(point: Point): T = grid(point.y)(point.x)

    def update(point: Point, value: T): Unit = grid(point.y)(point.x) = value

    def allPoints: Seq[Point] =
      for {
        y <- grid.indices
        x <- grid.head.indices
      } yield Point(x, y)

    def allPointsLazy: LazyList[Point] =
      for {
        y <- grid.indices.to(LazyList)
        x <- grid.head.indices.to(LazyList)
      } yield Point(x, y)

    def neighbours(point: Point): Seq[Point] = {
      val raw = Seq(Point(point.x - 1, point.y), Point(point.x, point.y - 1), Point(point.x + 1, point.y), Point(point.x, point.y + 1))
      raw.filter(p => p.x >= 0 && p.x < grid.head.length && p.y >= 0 && p.y < grid.length)
    }

    def allNeighbours(point: Point): Seq[Point] = {
      val raw = for {
        px <- point.x - 1 to point.x + 1
        py <- point.y - 1 to point.y + 1
      } yield Point(px, py)
      raw.filter(p => p.x >= 0 && p.x < grid.head.length && p.y >= 0 && p.y < grid.length)
    }

    def forall(pred: T => Boolean): Boolean = {
      grid.forall(_.forall(pred))
    }

    def count(pred: T => Boolean): Int = {
      allPoints.count { p => pred(value(p)) }
    }

    def findPoint(pred: T => Boolean): Option[Point] = {
      allPointsLazy.find(p => pred(value(p)))
    }

    override def equals(o2: Any): Boolean = {
      if (!o2.isInstanceOf[Grid[T]]) false else {
        val grid2 = o2.asInstanceOf[Grid[T]]
        width == grid2.width && height == grid2.height &&
          allPoints.forall { pt => value(pt) == grid2.value(pt) }
      }
    }
  }

  def iterate[T](start: T)(f: T => T): LazyList[T] = {
    def doIterate(newStart: T): LazyList[T] =
      newStart #:: doIterate(f(newStart))

    doIterate(start)
  }

  type FrequencyMap[T] = Map[T, Long]
  def FrequencyMap[T](): FrequencyMap[T] = Map[T, Long]()
  
  implicit class FrequencyMapOps[T](map: FrequencyMap[T]) {
    def addCount(el: T, count: Long): FrequencyMap[T] = {
      val newCount = count + map.getOrElse(el, 0L)
      map.updated(el, newCount)
    }
  }

  case class Interval(min: Int, max: Int) {
    def contains(x: Int): Boolean = x >= min && x <= max

    def size: Int = max - min + 1

    def intersect(i2: Interval): Option[Interval] = {
      if (min <= i2.min && max >= i2.min) {
        Some(Interval(i2.min, Math.min(max, i2.max)))
      } else
        if (min <= i2.max && min >= i2.min) {
          Some(Interval(min, Math.min(max, i2.max)))
        } else
          None
    }
  }
}
