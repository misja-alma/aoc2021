package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day15Part1 extends IOApp {

  case class Path(total: Int, point: Point)

  var count = 0

  def findShortestPath(grid: Grid[Int], startPoint: Point, endPoint: Point): Int = {
    val visited = mutable.Map[Point, Int]()
    visited.put(startPoint, 0)
    val queue = mutable.Queue[Path]()
    queue.enqueue(Path(0, startPoint))
    var best = Integer.MAX_VALUE
    
    while (queue.nonEmpty) {
      count += 1
      val p = queue.dequeue()
      if (p.point == endPoint) {
        if (p.total < best) best = p.total
      } else {
        val alreadyVisited = visited.getOrElse(p.point, Integer.MAX_VALUE)
        if (p.total == 0 || alreadyVisited > p.total) {
          visited.put(p.point, p.total)
          grid.neighbours(p.point).foreach { nb =>
            val newTotal = p.total + grid.value(nb)
            val alreadyVisited = visited.getOrElse(nb, Integer.MAX_VALUE)
            if (alreadyVisited > newTotal) {
              queue.enqueue(Path(newTotal, nb))
            }
          }
        }
      }
    }
    best
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day15.txt")
      lines = scannerToLines(sc).map(_.trim)
      grid = new Grid(lines.map(_.map(_.asDigit).toArray).toArray)
      shortestPath = findShortestPath(grid, Point(0, 0), Point(grid.width-1, grid.height-1))
      _ <- IO.delay(println("Solution: " + shortestPath + s" in $count iterations"))
      // only dequeued node check, >= total: 199,461,528
      // only dequeued node check, > total:  6,921,871
      // 2 checks, > total:                  3,402,764
      // with only enqueued node check it doesn't terminate at all!
    } yield ExitCode.Success
  }
}

object Day15Part2 extends IOApp {

  case class Path(total: Int, point: Point)

  object PathOrdering extends Ordering[Path] {
    def compare(p1: Path, p2: Path) = {
      p2.total.compareTo(p1.total)
    }  // reverse ordering for Scala's pq; it will put the largest in the head but we want the shortest path
  }

  // cells have min depth 1. So we can compare paths using total - manhattan distance and take the lowest
  class SmartPathOrdering(endPoint: Point) extends Ordering[Path] {
    def compare(p1: Path, p2: Path) = {
      val byTotal = p2.total.compareTo(p1.total)
      if (byTotal == 0) {
        Point.manhattanDistance(endPoint, p2.point).compareTo(Point.manhattanDistance(endPoint, p1.point))
      } else {
        byTotal
      }
      // just using this gives worse results! Is this because of the properties of the specific input?
      // (p2.total + Point.manhattanDistance(endPoint, p2.point)).compareTo(p1.total + Point.manhattanDistance(endPoint, p1.point))
    }  // reverse ordering for Scala's pq; it will put the largest in the head but we want the shortest path
  }

  var count = 0

  def findShortestPath(grid: Grid[Int], startPoint: Point, endPoint: Point): Int = {
    val visited = mutable.Map[Point, Int]()
    visited.put(startPoint, 0)
    val queue = mutable.PriorityQueue[Path]()(PathOrdering)
    queue.enqueue(Path(0, startPoint))
    var best = Integer.MAX_VALUE

    while (best == Integer.MAX_VALUE) {
      count += 1
      val p = queue.dequeue()
      if (p.point == endPoint) {
        if (p.total < best) best = p.total
      } else {
        visited.put(p.point, p.total)
        grid.neighbours(p.point).foreach { nb =>
          val newTotal = p.total + grid.value(nb)
          val alreadyVisited2 = visited.getOrElse(nb, Integer.MAX_VALUE)
          // for A* this check makes a huge difference .. Maybe because pq becomes slow as well with many nodes?
          if (alreadyVisited2 > newTotal) {
            visited.put(nb, newTotal)
            queue.enqueue(Path(newTotal, nb))
          }
        }
      }
    }
    best
  }

  def expandGrid(grid: Grid[Int]): Grid[Int] = {
    val newGrid = Grid.withDimensions[Int](grid.width * 5, grid.height * 5, 0)
    (0 until grid.width).foreach { orgX =>
      (0 until grid.height).foreach { orgY =>
        val orgPoint = Point(orgX,orgY)
        val orgValue = grid.value(orgPoint)
        (0 until 5).foreach { gridX =>
          (0 until 5).foreach { gridY =>
            val newPoint = Point(gridX * grid.width + orgX, gridY * grid.height + orgY)
            val newValueRaw = orgValue + gridX + gridY
            val newValue = if (newValueRaw > 9) newValueRaw - 9 else newValueRaw
            newGrid.update(newPoint, newValue)
          }
        }
      }
    }
    newGrid
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day15.txt")
      lines = scannerToLines(sc).map(_.trim)
      grid = new Grid(lines.map(_.map(_.asDigit).toArray).toArray)
      expandedGrid = expandGrid(grid)
      shortestPath = findShortestPath(expandedGrid, Point(0, 0), Point(expandedGrid.width-1, expandedGrid.height-1))
      _ <- IO.delay(println("Solution: " + shortestPath + s" in $count iterations"))    // 3025
      // 498999 iterations for expanded grid, 19,789 for original
      // smart ordering: 498999 and 19777
      // 265,572 with only enqueuing check + smart ordering, 10,451 for original , 9997 with normal ordering
      // 250000 with half-smart ordering, same as smart ordering
    } yield ExitCode.Success
  }
}