package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

import scala.annotation.tailrec

object Board {
  def apply(lines: Seq[String]): Board = {
    val nrs = lines.map(_.split("""(\s)+""").filterNot(_.isBlank).map(_.toInt).toArray).toArray
    val marked = nrs.map(_.map(_ => false))
    Board(nrs, marked)
  }
}

case class Board(numbers: Array[Array[Int]], marked: Array[Array[Boolean]]) {

  def unmarkedNumbers: Seq[Int] =
    for {
      row <- numbers.indices
      col <- numbers.head.indices
      if !marked(row)(col)
    } yield numbers(row)(col)

  def find(nr: Int): Option[(Int, Int)] = {
    val rowCols = for {
      row <- numbers.indices
      col <- numbers.head.indices
    } yield (row, col)
    rowCols.find { case (row, col) => numbers(row)(col) == nr }
  }

  def mark(nr: Int): Unit = {
    find(nr) match {
      case Some((row, col)) => marked(row)(col) = true
      case None => ()
    }
  }

  def isFinished: Boolean =
    marked.indices.exists { row => marked(row).forall(identity) } ||
      marked.head.indices.exists { col => marked.indices.forall { row => marked(row)(col) } }
}

object Day4Part1 extends IOApp {

  def parseBoards(lines: Seq[String]): Seq[Board] =
    lines match {
      case Nil => Seq()
      case line :: lns => if (line.isBlank) parseBoards(lns) else {
        val boardLines = lines.takeWhile(!_.isBlank)
        Board(boardLines) +: parseBoards(lns.drop(boardLines.size))
      }
    }

  @tailrec
  def findWinningBoard(boards: Seq[Board], nrs: Seq[Int]): (Board, Int) = {
    val nr = nrs.head
    boards.foreach(_.mark(nr))
    boards.find(_.isFinished) match {
      case Some(board) => (board, nr)
      case None => findWinningBoard(boards, nrs.tail)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day4.txt")
      lines = scannerToLines(sc)
      numbers = lines.head.split(",").map(_.toInt)
      boards = parseBoards(lines.tail)
      (board, nr) = findWinningBoard(boards, numbers)
      solution = board.unmarkedNumbers.sum * nr
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day4Part2 extends IOApp {

  def parseBoards(lines: Seq[String]): Seq[Board] =
    lines match {
      case Nil => Seq()
      case line :: lns => if (line.isBlank) parseBoards(lns) else {
        val boardLines = lines.takeWhile(!_.isBlank)
        Board(boardLines) +: parseBoards(lns.drop(boardLines.size))
      }
    }

  @tailrec
  def findLosingBoard(unfinishedBoards: Seq[Board], nrs: Seq[Int]): (Board, Int) = {
    val nr = nrs.head
    unfinishedBoards.foreach(_.mark(nr))
    val newUnfinished = unfinishedBoards.filterNot(_.isFinished)
    if (newUnfinished.isEmpty) {
      (unfinishedBoards.head, nr)
    } else {
      findLosingBoard(newUnfinished, nrs.tail)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day4.txt")
      lines = scannerToLines(sc)
      numbers = lines.head.split(",").map(_.toInt)
      boards = parseBoards(lines.tail)
      (board, nr) = findLosingBoard(boards, numbers)
      solution = board.unmarkedNumbers.sum * nr
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

