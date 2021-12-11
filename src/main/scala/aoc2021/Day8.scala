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
          case None => None // TODO we could still iterate here ..
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
    // for each candidate, take all digits, put them all in the possible locations ->
    // i.e. each parsed letter for the candidate gets a set of possible locations based on that number
    // do this for each number. But beware; nrs with multiple candidates create their own solution spaces! -> we have to iterate over those
    // each time we add a parsed digit to a nr candidate, we take the intersection of its possible locations with the possible ones
    // in the new candidate.
    // when this is done, we have for each parsed char the smallest set of possible locations
    // we have a number of these sets for the cartesian product of all nrs with multiple candidates
    // then for each of these;
    // no sets left: full solution!
    // identify a set of size 0. Found: no solution! Next candidate set
    // identify a set of size 1. Found: digit solution! Add to digit solutions of candidate set and remove location from all other sets.
    // no set of size 1 found? Oops, we need to extend our solution to iterate over possibilities ..
    // recurse
    val toIdentify = line.split(" ").toSet - "|"
    val differentDigits = toIdentify.flatten
    val possibleLocations = differentDigits.map(c => c -> (0 to 9).toSet).toMap
    val candidateSets = toIdentify.foldLeft(Seq(possibleLocations)) { case (candidateSets, pattern) => expandCandidateSets(candidateSets, pattern)}
    // not really needed but handy for debugging
    val nonEmpties = candidateSets.filterNot(_.exists(_._2.isEmpty))
    val nonDuplicates = nonEmpties.filterNot{s =>
      val ones = s.values.filter(_.size == 1) 
      ones.toSet.size < ones.size
    }

    // TODO would like some kind of mapFind, to stop early in case of a Some. Use stream (view?) maybe?
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
