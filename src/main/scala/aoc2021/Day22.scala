package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

object Day22Part1 extends IOApp {

  case class Interval3D(x: Interval, y: Interval, z: Interval, on: Boolean)

  def parseRange(line: String): Interval3D =
    line match {
      case  s"$on x=$xf..$xt,y=$yf..$yt,z=$zf..$zt" =>
        Interval3D(
          Interval(xf.toInt, xt.toInt),
          Interval(yf.toInt, yt.toInt),
          Interval(zf.toInt, zt.toInt),
          on == "on")
    }

  def scanPoints(ranges: Seq[Interval3D]): Set[Pos3D] = {
    ranges.foldLeft(Set[Pos3D]()) { case (total: Set[Pos3D], interval) =>
      val ptsForRange =
        for {
          x <- Math.max(-50, interval.x.min) to Math.min(50, interval.x.max)
          y <- Math.max(-50, interval.y.min) to Math.min(50, interval.y.max)
          z <- Math.max(-50, interval.z.min) to Math.min(50, interval.z.max)
        } yield Pos3D(x, y, z)
      ptsForRange.foldLeft(total) { case (adjustedTotal, pt) =>
        if (interval.on) {
          adjustedTotal + pt  
        } else {
          adjustedTotal - pt
        }
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day22.txt")
      ranges = scannerToLines(sc).map(parseRange)
      points = scanPoints(ranges)
      _ <- IO.delay(println("Solution: " + points.size))
    } yield ExitCode.Success
  }
}

object Day22Part2 extends IOApp {

  case class Interval3D(x: Interval, y: Interval, z: Interval, on: Boolean, weight: Long) {
    def volume: Long = if (on) weight * x.size * y.size * z.size else 0L
  }

  def parseRange(line: String): Interval3D =
    line match {
      case  s"$on x=$xf..$xt,y=$yf..$yt,z=$zf..$zt" =>
        Interval3D(
          Interval(xf.toInt, xt.toInt),
          Interval(yf.toInt, yt.toInt),
          Interval(zf.toInt, zt.toInt),
          on == "on",
          1)
    }

  def intersect(i1: Interval3D, i2: Interval3D, weight: Long): Option[Interval3D] = {
    for {
      x <- i1.x.intersect(i2.x)
      y <- i1.y.intersect(i2.y)
      z <- i1.z.intersect(i2.z)
    } yield Interval3D(x, y, z, true, weight)
  }

  def allCubes(ranges: Seq[Interval3D]): Seq[Interval3D] = {
    // add cubes but deduct intersections; add those as cubes with negative weight
    // intersections with earlier intersections get their weight reversed; etc
    // same for empty cubes except those are not added themselves to the total
    ranges.foldLeft(Seq[Interval3D]()) { case (total, r) =>
      val intersections = total.flatMap { t => intersect(t, r, -t.weight) }
      total ++ intersections ++ (if (r.on) Seq(r) else Seq())
    }
  }
  
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day22.txt")
      ranges = scannerToLines(sc).map(parseRange)
      cubes = allCubes(ranges)
      solution = cubes.map(_.volume).sum
        _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}
