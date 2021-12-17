package aoc2021

import aoc2021.Day16.{Literal, Operator, Packet, allVersionNrs, decode, hexToBinary}
import cats.effect.{ExitCode, IO, IOApp}

object Day16 {
  sealed trait Packet {
    def versionNr: Int
  }

  case class Literal(versionNr: Int, value: Long) extends Packet

  case class Operator(versionNr: Int, id: Int, subPackets: Seq[Packet]) extends Packet

  def allVersionNrs(packet: Packet): Seq[Int] =
    packet match {
      case Literal(nr, _) => Seq(nr)
      case Operator(nr, _, subPackets) => nr +: subPackets.flatMap(allVersionNrs)
    }

  def padLeadingZeroes(s: String, nrDigits: Int): String = s.reverse.padTo(nrDigits, '0').reverse

  def hexToBinary(line: String): Seq[Int] = {
    line.toSeq.flatMap { c =>
      val bString = if (c.isDigit) {
         c.asDigit.toBinaryString
      } else
        if (c >= 'A' && c <= 'F') {
          (c - 'A' + 10).toBinaryString
        } else {
          sys.error("Invalid hexadecimal : " + line)
        }
      padLeadingZeroes(bString, 4).map(_.asDigit)
    }
  }

  def bitsToLong(bits: Seq[Int]): Long =
    bits.foldLeft(0) { case (total, bit) => total * 2 + bit }

  def hexToLong(hex: Seq[Long]): Long =
    hex.foldLeft(0L) { case (total, digit) => total * 16 + digit }

  // first int = nr bits parsed, second is the value TODO I should do this with some monad
  def parseLiteralGroups(bits: Seq[Int]): (Int, Long) = {
    val firstParts = bits.sliding(5, 5).takeWhile(_.head == 1).toSeq
    val lastPartStart = firstParts.length * 5
    val lastPart = bits.slice(lastPartStart, lastPartStart + 5)
    val hex = firstParts.map(p => bitsToLong(p.tail)) :+ bitsToLong(lastPart.tail)
    val totalLength = (firstParts.length + 1) * 5
    (totalLength, hexToLong(hex))
  }

  def parseSubPackets(bits: Seq[Int]): (Int, Seq[Packet]) = {
    bits.head match {
      case 0 =>
        val subLength = bitsToLong(bits.slice(1, 16)).toInt
        val (parsedLength, subPackets) = parseSubPacketsByLength(bits.drop(16), subLength)
        (parsedLength + 1 + 15, subPackets)
      case 1 =>
        val subCount = bitsToLong(bits.slice(1, 12)).toInt
        val (parsedLength, subPackets) = parseSubPacketsByCount(bits.drop(12), subCount)
        (parsedLength + 1 + 11, subPackets)
    }
  }

  def parseSubPacketsByLength(bits: Seq[Int], length: Int): (Int, Seq[Packet]) = {
    var bitPos = 0
    var parsed = Seq[Packet]()
    while (bitPos < length) {
      val (parsedLength, packet) = decode(bits.drop(bitPos))
      parsed = parsed :+ packet
      bitPos += parsedLength.toInt
    }
    (length, parsed)
  }

  def parseSubPacketsByCount(bits: Seq[Int], count: Int): (Int, Seq[Packet]) = {
    // parse count packets
    var bitPos = 0
    var parsed = Seq[Packet]()
    (0 until count).foreach { _ =>
      val (length, packet) = decode(bits.drop(bitPos))
      parsed = parsed :+ packet
      bitPos += length
    }
    (bitPos, parsed)
  }

  def decode(bits: Seq[Int]): (Int, Packet) = {
    // 3 bits: version
    // 3 bits: ID. 4 = literal. followed by groups of 5 bits, 1st = 1 (more) or 0 (end).
    //             not-4 = operator id. followed by length id: 0 -> next 15 bits are total length in bits of subpackets
    //                                                         1 -> next 11 bits is nr of contained subpackets
    //                                  followed by subpackets
    val version = bitsToLong(bits.take(3)).toInt
    val id = bitsToLong(bits.slice(3, 6)).toInt
    id match {
      case 4 =>
        val (parsedLength, packet) = parseLiteralGroups(bits.drop(6))
        (parsedLength + 6, Literal(version, packet))
      case _ =>
        val (parsedLength, packet) = parseSubPackets(bits.drop(6))
        (parsedLength + 6, Operator(version, id, packet))
    }
  }

}

object Day16Part1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day16.txt")
      bits = hexToBinary(sc.nextLine().trim())
      (_, packet) = decode(bits)
      versionNrs = allVersionNrs(packet)
      _ <- IO.delay(println("Solution: " + versionNrs.sum))
    } yield ExitCode.Success
  }
}

object Day16Part2 extends IOApp {

  def evaluate(packet: Packet): Long =
    packet match {
      case Literal(_, value) => value
      case Operator(_, id, subPackets) =>
        val arguments = subPackets.map(evaluate)
        id match {
          case 0 => arguments.sum
          case 1 => arguments.product
          case 2 => arguments.min
          case 3 => arguments.max
          case 5 => if (arguments(0) > arguments(1)) 1L else 0L
          case 6 => if (arguments(0) < arguments(1)) 1L else 0L
          case 7 => if (arguments(0) == arguments(1)) 1L else 0L
        }
    }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day16.txt")
      bits = hexToBinary(sc.nextLine().trim())
      (_, packet) = decode(bits)
      result = evaluate(packet)
      _ <- IO.delay(println("Solution: " + result))  // 12301926782560
    } yield ExitCode.Success
  }
}


