package aoc2021

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Day8Part1 extends IOApp {

  def count1478(line: String): Map[Int, Int] = {
    // 1: 2 segments
    // 4: 4 segments
    // 7: 3
    // 8: 7
    line.split("""\|""").last // output
      .split(" ")
      .map(_.length)
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .filterKeys(s => s ==2 || s ==4 || s == 3 || s == 7)
      .toMap
  }
  
  def mergeCounts(count1: Map[Int, Int], count2: Map[Int, Int]): Map[Int, Int] = {
    val allKeys = (count1.keys ++ count2.keys).toSet
    allKeys.map { key => key -> (count1.getOrElse(key, 0) + count2.getOrElse(key, 0)) }.toMap
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day8.txt")
      lines = scannerToLines(sc)
      counts: Map[Int, Int] = lines.map(count1478).reduce(mergeCounts)
      solution = counts.values.sum
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object Day8Part2 extends IOApp {
  //   0
  //  1 2
  //   3
  //  4 5
  //   6

  val zero = Seq(0,1,2,4,5,6).toSet
  val one = Seq(2,5).toSet
  val two = Seq(0,2,3,4,6).toSet
  val three = Seq(0,2,3,5,6).toSet
  val four = Seq(1,2,3,5).toSet
  val five = Seq(0,1,3,5,6).toSet
  val six = Seq(0,1,3,4,5,6).toSet
  val seven = Seq(0,2,5).toSet
  val eight = Seq(0,1,2,3,4,5,6).toSet
  val nine = Seq(0,1,2,3,5,6).toSet
  val allNrs = List(zero, one, two, three, four, five, six, seven, eight, nine)

  def expandCandidateSets(cSets: Seq[Map[Char, Set[Int]]], str: String): Seq[Map[Char, Set[Int]]] = {
    val candidateNrs = allNrs.filter(_.size == str.length)
    candidateNrs.flatMap { cNrLocs =>
      val charsToLocs = str.map { c => c -> cNrLocs }.toMap
      cSets.map { cm => intersectMappings(charsToLocs, cm) }
    }
  }

  def intersectMappings(m1: Map[Char, Set[Int]], m2: Map[Char, Set[Int]]): Map[Char, Set[Int]] = {
    val newKeys = m1.keySet.intersect(m2.keySet)
    val unusedKeys1 = m1.keySet -- m2.keySet
    val unusedKeys2 = m2.keySet -- m1.keySet
    val intersections = newKeys.map { k => k -> m1(k).intersect(m2(k)) }.toMap
    intersections ++ m1.view.filterKeys(unusedKeys1).toMap ++ m2.view.filterKeys(unusedKeys2).toMap
  }

  def reduceToSolution(foundSolutions: Map[Char, Int], nrToBeFound: Int)(candidateSet: Map[Char, Set[Int]]): Option[Map[Char, Int]] = {
    if (candidateSet.values.exists(_.isEmpty)) None
    else
      if (foundSolutions.size == nrToBeFound) Some(foundSolutions)
      else {
        // identify single candidate; add to solution and eliminate; recurse
        candidateSet.find(_._2.size == 1) match {
          case None => None // Note in theory we could still iterate here ..
          case Some((key, value)) =>
            val newSolutions = foundSolutions + (key -> value.head)
            val newCandidateSet = (candidateSet - key).map { case (k, v) => k -> (v - value.head) }
            reduceToSolution(newSolutions, nrToBeFound)(newCandidateSet)
        }
      }
  }
  
  def toDigit(charMappings: Map[Char, Int])(chars: String): Int = {
    val charsAsLocs = chars.map(charMappings).toSet
    allNrs.zipWithIndex.find(_._1 == charsAsLocs).get._2
  }

  def digitsToInt(digits: Seq[Int]): Int =
    digits.foldLeft(0){ case (sum, d) => sum * 10 + d }

  def outputValue(line: String): Int = {
    val Array(_, output) = line.split("""\|""").map(_.trim)
    val charsToLocations = charMapping(line)
    val outputs = output.split(" ")
    val nrsInDigits: Array[Int] = outputs.map(toDigit(charsToLocations))
    digitsToInt(nrsInDigits)
  }

  def charMapping(line: String): Map[Char, Int] = {
    // number all digit-locations: 0..6
    // define per number its digit locations as a list of above indices
    // for each line: take the unique numbers
    // for each number, identify a list of candidates based on length
    val toIdentify = line.split(" ").toSet - "|"
    val differentDigits = toIdentify.flatten
    val possibleLocations = differentDigits.map(c => c -> (0 to 9).toSet).toMap
    // Start with adding all locations in the matching digit(s) based on length to each character in the string
    // intersect the possible locations per new matching digit
    val candidateSets = toIdentify.foldLeft(Seq(possibleLocations)) { case (candidateSets, pattern) => expandCandidateSets(candidateSets, pattern)}
    // not really needed but handy for debugging
    val nonEmpties = candidateSets.filterNot(_.exists(_._2.isEmpty))
    val nonDuplicates = nonEmpties.filterNot{s =>
      val ones = s.values.filter(_.size == 1) 
      ones.toSet.size < ones.size
    }

    // then reduce the possible location sets per character by each time taking a singleton set and removing it from all other
    // candidate sets, until a solution is found or a candidate set turns out to be impossible
    val solution = nonDuplicates.view.map(c => reduceToSolution(Map(), differentDigits.size)(c)).find(_.isDefined).get
    solution.get
  }

  def mergeCounts(count1: Map[Int, Int], count2: Map[Int, Int]): Map[Int, Int] = {
    val allKeys = (count1.keys ++ count2.keys).toSet
    allKeys.map { key => key -> (count1.getOrElse(key, 0) + count2.getOrElse(key, 0)) }.toMap
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day8.txt")
      lines = scannerToLines(sc)
      solution = lines.map(outputValue).sum
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}
