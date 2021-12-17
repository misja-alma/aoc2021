package aoc2021

import aoc2021.Day17.parseTargetArea
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Day17 {

  private val pattern = """target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)""".r
  def parseTargetArea(str: String): (Int, Int, Int, Int) =
    str match {
      case pattern(xm, xh, ym, yh) => (xm.toInt, xh.toInt, ym.toInt, yh.toInt)
    }
}

object Day17Part1 extends IOApp {

  def findHighestSolution(xm: Int, xh: Int, ym: Int, yh: Int): Int = {

    def findTrajectory(start: Point, vx: Int, vy: Int): Option[Seq[Point]] = {
      val Point(x, y) = start
      if (x >= xm && x <= xh && y >= ym && y <= yh) {
        Some(Seq(start))
      } else {
        if ((vx <= 0 && x < xm) || y < ym) None
        else {
          val newX = x + vx
          val newY = y + vy
          val newVx = vx + (if (vx > 0) -1 else if (vx < 0) 1 else 0)
          val newVy = vy - 1
          findTrajectory(Point(newX, newY), newVx, newVy).map(pts => start +: pts)
        }
      }
    }

    val startingPos = Point(0, 0)

    val allSolutions: Seq[Seq[Point]] = (0 to 500).flatMap { vy =>
      val allForY = (1 to xh).flatMap { vx =>
        findTrajectory(startingPos, vx, vy)
      }
      if (allForY.isEmpty) None else Some(allForY.maxBy(_.map(_.y).max))
    }
    allSolutions.map(_.maxBy(_.y).y).max
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day17.txt")
      (xm, xh, ym, yh) = parseTargetArea(sc.nextLine())
      solution = findHighestSolution(xm, xh, ym, yh)
      _ <- IO.delay(println("Solution: " + solution))  
    } yield ExitCode.Success
  }
}

object Day17Part2 extends IOApp {

  def findAllSolutions(xm: Int, xh: Int, ym: Int, yh: Int): Set[(Int, Int)] = {

    def findTrajectory(start: Point, vx: Int, vy: Int): Option[Seq[Point]] = {
      val Point(x, y) = start
      if (x >= xm && x <= xh && y >= ym && y <= yh) {
        Some(Seq(start))
      } else {
        if ((vx <= 0 && x < xm) || y < ym) None
        else {
          val newX = x + vx
          val newY = y + vy
          val newVx = vx + (if (vx > 0) -1 else if (vx < 0) 1 else 0)
          val newVy = vy - 1
          findTrajectory(Point(newX, newY), newVx, newVy).map(pts => start +: pts)
        }
      }
    }

    val startingPos = Point(0, 0)

    val allSolutions = (-500 to 500).flatMap { vy =>
      val allForY: Seq[(Int, Int)] = (1 to xh).flatMap { vx =>
        findTrajectory(startingPos, vx, vy).map(_ => (vx, vy))
      }
      allForY.toSet
    }
    allSolutions.toSet
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day17.txt")
      (xm, xh, ym, yh) = parseTargetArea(sc.nextLine())
      solution = findAllSolutions(xm, xh, ym, yh)
      _ <- IO.delay(println("Solution: " + solution.size))
    } yield ExitCode.Success
  }
}
