package aoc2021

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

case class Position(forward: Int, aim: Int, depth: Int)

object Day2Part1 extends IOApp {
  val forward = """forward (\d*)""".r
  val up = """up (\d*)""".r
  val down = """down (\d*)""".r

  type Move = Position => Position

  def parseMove(s: String): Either[IllegalArgumentException, Move] = s match {
    case forward(dx) => Right(pos => pos.copy(forward = pos.forward + dx.toInt))
    case up(dz) =>      Right(pos => pos.copy(depth = pos.depth - dz.toInt))
    case down(dz) =>    Right(pos => pos.copy(depth = pos.depth + dz.toInt))
    case _ =>           Left(new IllegalArgumentException("Unknown move: " + s))
  }

  def printLists(list: Seq[Seq[AnyVal]]) =
    for {
      subList: List[Int] <- list
    } yield {
      println (subList.mkString)
    }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day2.txt")
      movesOrError = scannerToLines(sc).traverse(parseMove)
      moves <- movesOrError.liftTo[IO] // TODO check: explicit type generates a withFilter! Check if this really filters, i.e. when using simple lists
      result = moves.foldLeft(Position(0, 0, 0)) { case (pos, cmd) => cmd(pos) }
      _ <- IO.delay(println("Solution: " + result.forward * result.depth))
    } yield ExitCode.Success
  }
}

