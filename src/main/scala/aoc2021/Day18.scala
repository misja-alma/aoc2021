package aoc2021

import aoc2021.Day18.{Pair, addPairs, magnitude, parsePairs}
import cats.Foldable
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day18 {
  sealed trait Pair {
    def print: String
  }

  case class PairLeaf(value: Long) extends Pair {
    override def print: String = value.toString
  }

  case class PairNode(left: Pair, right: Pair) extends Pair {
    override def print: String = s"[${left.print},${right.print}]"
  }

  def isOpeningDelimiter(char: Char): Boolean = Set('[').contains(char)

  def isClosingDelimiter(char: Char): Boolean = Set(']').contains(char)

  def parsePairs(line: String): Pair = {
    val stack = mutable.Stack[Pair]()
    Foldable[Seq].foldM(line, stack) { case (stack, char) =>
      if (isOpeningDelimiter(char) || char == ',') {
        //stack.push(char)
        Right(stack) // start of block or comma; do nothing
      } else if (isClosingDelimiter(char)) {
        if (stack.isEmpty) Left("Stack unexpectedly empty") // incomplete from right
        else {
          val lastRight = stack.pop()
          val lastLeft = stack.pop()
          val pair = PairNode(lastLeft, lastRight)
          stack.push(pair)
          Right(stack)
        }
      } else {
        val value = char.asDigit
        val pair = PairLeaf(value)
        stack.push(pair)
        Right(stack)
      } // digit
    } match {
      case Left(error) => sys.error(error)
      case Right(stackWithPair) =>
        if (stackWithPair.size > 1) sys.error("Stack not fully processed! " + stackWithPair)
        stackWithPair.pop()
    }
  }

  def split(pair: Pair): Option[Pair] =
    pair match {
      case PairLeaf(value) =>
        if (value >= 10) {
          val newLeft = value / 2
          val newRight = (value + 1) / 2
          Some(PairNode(PairLeaf(newLeft), PairLeaf(newRight)))
        } else None
      case PairNode(left, right) =>
        split(left) match {
          case Some(newLeft) => Some(PairNode(newLeft, right))
          case None =>
            split(right) match {
              case Some(newRight) => Some(PairNode(left, newRight))
              case None => None
            }
        }
    }

  def addToLeft(pair: Pair, x: Long): Pair =
    pair match {
      case PairLeaf(value) => PairLeaf(value + x)
      case PairNode(left, right) => PairNode(addToLeft(left, x), right)
    }

  def addToRight(pair: Pair, x: Long): Pair =
    pair match {
      case PairLeaf(value) => PairLeaf(value + x)
      case PairNode(left, right) => PairNode(left, addToRight(right, x))
    }

  def explode(pair: Pair): Option[Pair] = {
    // find explode; explode and check results immediately; split them if needed

    // left: needs explode. right: not
    def doExplode(pair: Pair, depth: Int): (Boolean, Option[Long], Option[Long], Pair) = {
      pair match {
        case pl@PairLeaf(_) =>
          (false, None, None, pl) // leafs don't need to be exploded
        case p@PairNode(left, right) =>
          if (depth >= 4) {
            // explode
            // the pair will cease to exist. Both values go to the parent who
            // should add them somehow
            // NOTE: we assume this should always be a node with only leafnodes as children!
            if (left.isInstanceOf[PairNode] || right.isInstanceOf[PairNode]) sys.error("Pairnodes at depth 4!")
            (true, Some(left.asInstanceOf[PairLeaf].value), Some(right.asInstanceOf[PairLeaf].value), p)
          } else {
            doExplode(left, depth + 1) match {
              case (exploded, al@Some(_), Some(addRight), _) =>
                // explosion just happened. Reset the node
                // we can't handle the left, that has to go one more up
                // the right needs to be added to the right node of this pair,
                //  and no further explode checks needed
                val newLeft = PairLeaf(0)
                val newRight = addToLeft(right, addRight) // Note: we want the leftmost of right here!
                (exploded, al, None, PairNode(newLeft, newRight))
              case (exploded, al@Some(_), None, changedLeft) =>
                // explosion happened somewhere below. Finish, just try to add left to some parent
                (exploded, al, None, PairNode(changedLeft, right))
              case (exploded, None, Some(addRight), changedLeft) =>
                // explosion happened somewhere below. Finish, just add right
                // Note: we want the leftmost of right here! So when it comes here, add left
                val newRight = addToLeft(right, addRight)
                (exploded, None, None, PairNode(changedLeft, newRight))
              case (exploded, None, None, maybeChangedLeft) =>
                // perhaps nothing happened, keep looking for explodes in the right
                // we only have to check for explodes if the left hasn't exploded yet!
                // otherwise we have to add it
                if (!exploded) {
                  doExplode(right, depth + 1) match {
                    case (exploded, Some(addLeft), ar@Some(_), _) =>
                      // we can't handle the right, that has to go one more up
                      val newRight = PairLeaf(0)
                      val newLeft = addToRight(maybeChangedLeft, addLeft) // Note: we want the rightmost of left here!
                      (exploded, None, ar, PairNode(newLeft, newRight))
                    case (exploded, None, ar@Some(_), changedRight) =>
                      // explosion happened somewhere below. Finish, just try to add right to some parent
                      (exploded, None, ar, PairNode(maybeChangedLeft, changedRight))
                    case (exploded, Some(addLeft), None, changedRight) =>
                      // explosion happened somewhere below. Finish, just add left
                      // Note: we want the rightmost of left here! So when it somes here, add it to right
                      val newLeft = addToRight(maybeChangedLeft, addLeft)
                      (exploded, None, None, PairNode(newLeft, changedRight))
                    case (exploded, None, None, maybeChangedRight) =>
                      (exploded, None, None, PairNode(maybeChangedLeft, maybeChangedRight))
                  }
                } else {
                  // don't continue when exploded
                  (exploded, None, None, PairNode(maybeChangedLeft, right))
                }
            }
          }
      }
    }

    val (hasExploded, _, _, finalPair) = doExplode(pair, 0)

    if (!hasExploded) None else Some(finalPair)
  }

  def reducePair(pair: Pair): Pair = {
    // first explodes; then splits; repeat until no more explodes after splits
    var finished = false
    var reducedPair = pair

    while (!finished) {
      finished = true
      // keep exploding: explodes go first
      var newExplode: Option[Pair] = Some(reducedPair)
      var lastExplode = reducedPair
      while (newExplode.isDefined) {
        lastExplode = newExplode.get
        newExplode = explode(lastExplode)
        if (newExplode.isDefined) {
          finished = false
        }
      }

      // do at most one split because it might cause new explodes which go first
      reducedPair = split(lastExplode) match {
        case Some(newPair) =>
          finished = false // split happened, we're not finished yet
          newPair
        case None =>
          lastExplode
      }
    }

    reducedPair
  }

  def addPairs(p1: Pair, p2: Pair): Pair = {
    val newPair = PairNode(p1, p2)
    reducePair(newPair)
  }

  def magnitude(pair: Pair): Long =
    pair match {
      case PairLeaf(value) => value
      case PairNode(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
    }
}

object Day18Part1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day18.txt")
      pairs = scannerToLines(sc).map(parsePairs)
      summed = pairs.reduce(addPairs)
      _ <- IO.delay(println("summed: " + summed.print))
      solution = magnitude(summed)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day18Part2 extends IOApp {

  def listAllSums(pairs: Seq[Pair]): Seq[Pair] = {
    for {
      p1 <- pairs
      p2 <- pairs
      if p1 != p2
      sum <- Seq(addPairs(p1, p2), addPairs(p2, p1))
    } yield sum
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day18.txt")
      pairs = scannerToLines(sc).map(parsePairs)
      allSums = listAllSums(pairs).map(magnitude)
      solution = allSums.max
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}
