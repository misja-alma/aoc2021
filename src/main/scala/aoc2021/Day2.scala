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

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day2.txt")
      movesOrError = scannerToLines(sc).traverse(parseMove)
      moves <- movesOrError.liftTo[IO]
      result = moves.foldLeft(Position(0, 0, 0)) { case (pos, cmd) => cmd(pos) }
      _ <- IO.delay(println("Solution: " + result.forward * result.depth))
    } yield ExitCode.Success
  }
}

object Day2Part2 extends IOApp {
  val forward = """forward (\d*)""".r
  val up = """up (\d*)""".r
  val down = """down (\d*)""".r

  type Move = Position => Position

  def parseMove(s: String): Either[IllegalArgumentException, Move] = s match {
    case forward(dx) => Right(pos => pos.copy(forward = pos.forward + dx.toInt, depth = pos.depth + pos.aim * dx.toInt))
    case up(dz) =>      Right(pos => pos.copy(aim = pos.aim - dz.toInt))
    case down(dz) =>    Right(pos => pos.copy(aim = pos.aim + dz.toInt))
    case _ =>           Left(new IllegalArgumentException("Unknown move: " + s))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day2.txt")
      movesOrError = scannerToLines(sc).traverse(parseMove)
      moves <- movesOrError.liftTo[IO]
      result = moves.foldLeft(Position(0, 0, 0)) { case (pos, cmd) => cmd(pos) }
      _ <- IO.delay(println("Solution: " + result.forward * result.depth))
    } yield ExitCode.Success
  }
}
