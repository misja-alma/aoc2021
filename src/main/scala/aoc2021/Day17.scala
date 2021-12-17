package aoc2021

import aoc2021.Day17.{findFunction, parseTargetArea}
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Day17 {

  private val pattern = """target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)""".r
  def parseTargetArea(str: String): (Interval, Interval) =
    str match {
      case pattern(xm, xh, ym, yh) => (Interval(xm.toInt, xh.toInt), Interval(ym.toInt, yh.toInt))
    }

  def findFunction(rangeX: Interval, rangeY: Interval): (Point, Int, Int) => Option[Seq[Point]] = {

    def findTrajectory(start: Point, vx: Int, vy: Int): Option[Seq[Point]] = {
      val Point(x, y) = start
      if (rangeX.contains(x) && rangeY.contains(y)) {
        Some(Seq(start))
      } else {
        if ((vx <= 0 && x < rangeX.min) || y < rangeY.min) None
        else {
          val newX = x + vx
          val newY = y + vy
          val newVx = vx + (if (vx > 0) -1 else if (vx < 0) 1 else 0)
          val newVy = vy - 1
          findTrajectory(Point(newX, newY), newVx, newVy).map(pts => start +: pts)
        }
      }
    }

    findTrajectory _
  }
}

object Day17Part1 extends IOApp {

  def findHighestSolution(rangeX: Interval, rangeY: Interval) = { //xm: Int, xh: Int, ym: Int, yh: Int): Int = {

    val find = findFunction(rangeX, rangeY)
    val startingPos = Point(0, 0)

    val allSolutions =
      for {
        vy <- 0 to 500
        vx <- 1 to rangeX.max
        allForY <- find(startingPos, vx, vy)
      } yield  allForY.maxBy(_.y)

    allSolutions.maxBy(_.y).y
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day17.txt")
      (rangeX, rangeY) = parseTargetArea(sc.nextLine())
      solution = findHighestSolution(rangeX, rangeY)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day17Part2 extends IOApp {

  def findAllSolutions(rangeX: Interval, rangeY: Interval): Set[(Int, Int)] = {

    val find = findFunction(rangeX, rangeY)
    val startingPos = Point(0, 0)

    val allSolutions = for {
      vy <- -500 to 500
      vx <- 1 to rangeX.max
    } yield find(startingPos, vx, vy).map(_ => (vx, vy))
    
    allSolutions.flatten.toSet
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day17.txt")
      (rangeX, rangeY) = parseTargetArea(sc.nextLine())
      solution = findAllSolutions(rangeX, rangeY)
      _ <- IO.delay(println("Solution: " + solution.size))
    } yield ExitCode.Success
  }
}
