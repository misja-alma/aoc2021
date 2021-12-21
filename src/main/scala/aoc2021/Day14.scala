package aoc2021

import aoc2021.Day14._
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.immutable._

object Day14 {
  def toEdges(line: String): (String, Char) = line match {
    case s"$from -> $to" => from -> to.head
    case _ => sys.error("Can't parse line: " + line)
  }
  
  def countChars(s: String): FrequencyMap[Char] =
    s.groupBy(identity).view.mapValues(_.length.toLong).toMap

  case class Counts(pairs: FrequencyMap[String], chars: FrequencyMap[Char])

  def generateNewPairs(mappings: Map[String, Char])(counts: Counts): Counts = {
    counts.pairs.foldLeft(Counts(Map[String, Long](), counts.chars)) { case (total, (pair, count)) =>
      val mapping = mappings(pair)
      val newPairs = Seq(s"${pair.head}$mapping", s"$mapping${pair.last}")
      val newPairCounts = newPairs.foldLeft(total.pairs) { case (newTotal, newPair) => newTotal.addCount(newPair, count)}
      val newCharCounts = total.chars.addCount(mapping, count)
      Counts(newPairCounts, newCharCounts)
    }
  }
}

object Day14Part1And2 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day14.txt")
      lines = scannerToLines(sc)
      start = lines.head
      mappings = lines.drop(2).map(toEdges).toMap
      pairs = start.sliding(2, 1).foldLeft(FrequencyMap[String]()){ case (set, pair) => set.addCount(pair, 1) }
      counts = countChars(start)
      generations = LazyList.iterate(Counts(pairs, counts)) { c =>
        generateNewPairs(mappings)(c)
      }
      pairsAfter40 = generations(40)  // change to 10 for part 1
      charCounts = pairsAfter40.chars.values
      solution = charCounts.max - charCounts.min
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

