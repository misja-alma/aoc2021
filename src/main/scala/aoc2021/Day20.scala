package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

object Day20Part1 extends IOApp {

  def neighbours(point: Point): Seq[Point] =
    for {
      px <- point.x-1 to point.x+1
      py <- point.y-1 to point.y+1
    } yield Point(px, py)

  def bitsToInt(bits: Seq[Int]): Int =
    bits.foldLeft(0) { case (total, bit) => total * 2 + bit }
  
  def calcOutput(index: Array[Boolean])(inputState: (Boolean, Set[Point])): (Boolean, Set[Point]) = {
    val (emptyIsLight: Boolean, input: Set[Point]) = inputState
    val newPointsToConsider = input.flatMap(neighbours)
    val newPoints = newPointsToConsider.flatMap { n=>
      // when empty is light, we are only interested in empty points, otherwise in bright points
      // empty is light means that if a point is in the set, it is dark => should be 0
      val cells = neighbours(n).map { nn => if (input(nn) ^ emptyIsLight) 1 else 0 }
      val i = bitsToInt(cells)
      // when empty is light, next time empty is dark. So we are interested in bright cells then
      if (index(i) ^ emptyIsLight) None else Some(n)
    }
    (!emptyIsLight, newPoints)
  }
  
  def parseInput(lines: Seq[String]): (Array[Boolean], Set[Point]) = {
    val index = lines.takeWhile(_.nonEmpty).mkString("").map(_ == '#').toArray
    val gridLines = lines.dropWhile(_.nonEmpty).dropWhile(_.isEmpty)
    val grid = gridLines.map{line => line.map(_ == '#').toArray}.toArray
    val points = for {
      y <- grid.indices
      x <- grid.head.indices
    } yield {
      if (grid(y)(x)) Some(Point(x, y)) else None
    }
    (index, points.flatten.toSet)
  }
  
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day20.txt")
      lines = scannerToLines(sc)
      (index, input) = parseInput(lines)
      // in gen 0 empty cells are dark, in gen 1 reverse etc
      (_, finalInput) = LazyList.iterate((false, input))(calcOutput(index))(50)  // change to 2 for part 1
      solution = finalInput.size
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}
