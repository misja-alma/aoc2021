package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day21Part1 extends IOApp {

  def parsePosition(line: String): Int =
    line match {
      case s"Player $_ starting position: $pos" => pos.toInt
    }

  case class GameState(diceRolls: Int, p1: Int, p2: Int, p1Score: Long, p2Score: Long, lastDie: Int) {
    def losingScore: Option[Long] = if (p1Score >= 1000) Some(p2Score) else if (p2Score >= 1000) Some(p1Score) else None
  }

  def roll3(lastDie: Int): (Int, Int) = {
    val nextDie1 = (lastDie+1) % 100
    val nextDie2 = (nextDie1+1) % 100
    val nextDie3 = (nextDie2+1) % 100
    (nextDie3, nextDie1 + nextDie2 + nextDie3)  
  }

  // just brute force
  def nextRoll(gs: GameState): GameState = {
    val (nextDie, totalRoll) = roll3(gs.lastDie)
    val nextP1 = (gs.p1 + totalRoll - 1) % 10 + 1
    val nextP1Score = gs.p1Score + nextP1
    if (nextP1Score >= 1000) GameState(gs.diceRolls + 3, nextP1, gs.p2, nextP1Score, gs.p2Score, nextDie)
    else {
      val (nextDie2, totalRoll2) = roll3(nextDie)
      val nextP2 = (gs.p2 + totalRoll2 - 1) % 10 + 1
      val nextP2Score = gs.p2Score + nextP2
      GameState(gs.diceRolls + 6, nextP1, nextP2, nextP1Score, nextP2Score, nextDie2)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day21.txt")
      lines = scannerToLines(sc)
      Seq(pos1, pos2) = lines.map(parsePosition)
      startState = GameState(0, pos1, pos2, 0, 0, 0)
      finalState = iterate(startState)(nextRoll).dropWhile(_.losingScore.isEmpty).head
      solution = finalState.losingScore.get * finalState.diceRolls
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day21Part2 extends IOApp {

  def parsePosition(line: String): Int =
    line match {
      case s"Player $_ starting position: $pos" => pos.toInt
    }

  def rollDirac: Seq[Int] = {
    for {
      die1 <- 1 to 3
      die2 <- 1 to 3
      die3 <- 1 to 3
    } yield (die1 + die2 + die3)
  }

  case class GameState2(p1Pos: Int, p1Score: Int, p2Pos: Int,  p2Score: Int, p1OnRoll: Boolean)

  val stateMap = mutable.Map[GameState2, (Long, Long)]()

  // traverse the graph of universes until a winner is found, use a map of known ones
  def finishGame(state: GameState2): (Long, Long) = {
    stateMap.get(state) match {
      case Some(wins) => wins
      case None =>
        if (state.p1Score >= 21) (1, 0)
        else if (state.p2Score >= 21) (0, 1) else {
          // otherwise generate dice rolls, recurse for each of them, sum the results
          val p1p2Wins = rollDirac.map { total =>
            val newState = if (state.p1OnRoll) {
              val nextP1Pos = (state.p1Pos + total - 1) % 10 + 1
              val nextP1Score = state.p1Score + nextP1Pos
              GameState2(nextP1Pos, nextP1Score, state.p2Pos, state.p2Score, !state.p1OnRoll)
            } else {
              val nextP2Pos = (state.p2Pos + total - 1) % 10 + 1
              val nextP2Score = state.p2Score + nextP2Pos
              GameState2(state.p1Pos, state.p1Score, nextP2Pos, nextP2Score, !state.p1OnRoll)
            }
            finishGame(newState)
          }

          val summedWins = p1p2Wins.foldLeft((0L, 0L)){ case ((tw1, tw2), (w1, w2)) => (tw1 + w1, tw2 + w2)}
          stateMap.update(state, summedWins)

          summedWins
        }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day21.txt")
      lines = scannerToLines(sc)
      Seq(pos1, pos2) = lines.map(parsePosition)
      startState = GameState2(pos1, 0, pos2, 0, p1OnRoll = true)
      finalState = finishGame(startState)
      solution = Math.max(finalState._1, finalState._2)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}
