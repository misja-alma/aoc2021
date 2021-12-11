package aoc2021

import cats.Foldable
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.collection.mutable
import scala.util.Either


object Day10Part1 extends IOApp {

  def isOpeningDelimiter(char: Char): Boolean = Set('(', '{', '[', '<').contains(char)

  def isClosingDelimiter(char: Char): Boolean = Set(')', '}', ']', '>').contains(char)

  def matches(opening: Char, closing: Char): Boolean = closing match {
    case ')' => opening == '('
    case ']' => opening == '['
    case '}' => opening == '{'
    case '>' => opening == '<'
    case _ => false
  }

  def findCorruptDelimiter(line: String): Option[Char] = {
    // for each opening delimiter: push on a stack
    // for each closing delimiter: pop the stack: it should match
    val stack = mutable.Stack[Char]()
    Foldable[Seq].foldM(line, stack) { case (stack, char) =>
      if (isOpeningDelimiter(char)) {
        stack.push(char)
        Right(stack)   // start of block
      } else
        if (isClosingDelimiter(char)) {
          if (stack.isEmpty) Left(None) // incomplete from right
          else {
            val lastOpening = stack.pop()
            if (matches(lastOpening, char)) {
              Right(stack)  // end of block
            } else {
              Left(Some(char)) // corrupt
            }
          }
        } else Right(stack) // other char
    } match {
      case Left(maybeCorruptChar) => maybeCorruptChar
      case Right(_) => None
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day10.txt")
      lines = scannerToLines(sc)
      corruptDelimiters = lines.flatMap(findCorruptDelimiter)
      scores = corruptDelimiters map {
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
        case x => sys.error("Not corrupt char: " + x)
      }
      _ <- IO.delay(println("Solution: " + scores.sum))
    } yield ExitCode.Success
  }
}

object Day10Part2 extends IOApp {

  def isOpeningDelimiter(char: Char): Boolean = Set('(', '{', '[', '<').contains(char)

  def isClosingDelimiter(char: Char): Boolean = Set(')', '}', ']', '>').contains(char)

  def matches(opening: Char, closing: Char): Boolean = closing match {
    case ')' => opening == '('
    case ']' => opening == '['
    case '}' => opening == '{'
    case '>' => opening == '<'
    case _ => false
  }

  def findIncompleteMissingDelimiters(line: String): Option[Seq[Char]] = {
    // for each opening delimiter: push on a stack
    // for each closing delimiter: pop the stack: it should match
    val stack = mutable.Stack[Char]()
    Foldable[Seq].foldM(line, stack) { case (stack, char) =>
      if (isOpeningDelimiter(char)) {
        stack.push(char)
        Right(stack)   // start of block
      } else
        if (isClosingDelimiter(char)) {
          if (stack.isEmpty) Left(None) // incomplete from right
          else {
            val lastOpening = stack.pop()
            if (matches(lastOpening, char)) {
              Right(stack)  // end of block
            } else {
              Left(Some(char)) // corrupt
            }
          }
        } else Right(stack) // other char
    } match {
      case Left(_) => None
      case Right(stack) => if (stack.isEmpty) None else Some(stack.toSeq)
    }
  }

  def scoreIncomplete(opening: Seq[Char]): Long = {
    opening.foldLeft(0L){ case (score, char) =>
       score * 5 + (char match {
         case '(' => 1
         case '[' => 2
         case '{' => 3
         case '<' => 4
         case _ => sys.error("Incorrect opening char: " + char)
       })
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day10.txt")
      lines = scannerToLines(sc)
      incomplete = lines.flatMap(findIncompleteMissingDelimiters)
      scores = incomplete.map(scoreIncomplete)
      sortedScores = scores.sorted
      _ <- IO.delay(println("Solution: " + sortedScores(scores.size/2)))
    } yield ExitCode.Success
  }
}
