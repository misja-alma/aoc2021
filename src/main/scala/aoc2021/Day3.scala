package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

import scala.annotation.tailrec

object Day3Part1 extends IOApp {

  def bitStringToInt(bits: Seq[Char]): Int =
    bits.foldLeft(0){ case (total, bit) => total * 2 + bit.asDigit }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day3.txt")
      lines = scannerToLines(sc)
      bitStrings = lines.transpose
      bitCounts = bitStrings.map(_.groupBy(identity))
      maxOccurring = bitCounts.map(_.maxBy(_._2.size)._1)
      minOccurring = bitCounts.map(_.minBy(_._2.size)._1)
      gamma = bitStringToInt(maxOccurring)
      epsilon = bitStringToInt(minOccurring)
      _ <- IO.delay(println("Solution: " + gamma * epsilon))
    } yield ExitCode.Success
  }
}

object Day3Part2 extends IOApp {

  def bitStringToInt(bits: Seq[Char]): Int =
    bits.foldLeft(0){ case (total, bit) => total * 2 + bit.asDigit }

  // for each bit:
  // - select most/least common
  // - filter nrs having this bit in current pos
  // stop when only one nr left. Note: will this always happen?
  @tailrec
  def selectByCriterium(lines: Seq[String], bitCountCriterium: (Int, Int) => Char, bitPos: Int = 0): Seq[Char] = {
    if (lines.size == 1) lines.head
    else {
      val bitCounts = lines.map(_(bitPos)).groupBy(identity).view.mapValues(_.size)
      val minBit = if (bitCounts.size == 1) bitCounts.keys.head else bitCountCriterium(bitCounts('1'), bitCounts('0'))
      val newStrings = lines.filter(_(bitPos) == minBit)
      selectByCriterium(newStrings, bitCountCriterium, bitPos + 1)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day3.txt")
      lines = scannerToLines(sc)
      selectedByMax = selectByCriterium(lines, { (ones, zeroes) => if (ones >= zeroes) '1' else '0' } )
      selectedByMin = selectByCriterium(lines, { (ones, zeroes) => if (ones < zeroes) '1' else '0' } )
      gamma = bitStringToInt(selectedByMax)
      epsilon = bitStringToInt(selectedByMin)
      _ <- IO.delay(println("Solution: " + gamma * epsilon))    
    } yield ExitCode.Success
  }
}
