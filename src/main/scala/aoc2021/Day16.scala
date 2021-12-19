package aoc2021

import aoc2021.Day16.PacketParser.packet
import aoc2021.Day16.{Literal, Operator, Packet, PacketParser, allVersionNrs, hexToBinary}
import cats.effect.{ExitCode, IO, IOApp}

import scala.util.parsing.combinator._

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

  def hexToBinary(line: String): String = {
    line.toSeq.flatMap { c =>
      val bString = if (c.isDigit) {
         c.asDigit.toBinaryString
      } else
        if (c >= 'A' && c <= 'F') {
          (c - 'A' + 10).toBinaryString
        } else {
          sys.error("Invalid hexadecimal : " + line)
        }
      padLeadingZeroes(bString, 4)
    }.mkString("")
  }

  def bitsToInt(bits: String): Int =
    bits.foldLeft(0) { case (total, bit) => total * 2 + bit.asDigit }

  def bitsToLong(bits: Seq[Int]): Long =
    bits.foldLeft(0) { case (total, bit) => total * 2 + bit }
  
  def hexToLong(hex: Seq[Int]): Long =
    hex.foldLeft(0L) { case (total, digit) => total * 16 + digit }

  object PacketParser extends RegexParsers {
    def version: Parser[Int] = """[0,1]{3}""".r ^^ { bitsToInt }

    def literalPacketId = "100"
    def operatorPacketId: Parser[Int] = """(000)|(001)|(010)|(011)|(101)|(110)|(111)""".r ^^ { bitsToInt }

    def subPacket : Parser[Int] = """1[0,1]{4}""".r ^^ { s => bitsToInt(s.tail) }
    def terminalSubPacket : Parser[Int] = """0[0,1]{4}""".r ^^ { s => bitsToInt(s.tail) }
    def packetValue: Parser[Long] = rep(subPacket) ~ terminalSubPacket ^^ { case subPackets ~ terminal => hexToLong(subPackets :+ terminal) }

    def literalPacket: Parser[Literal] = version ~ literalPacketId ~ packetValue ^^ { case version ~ id ~ value => Literal(version, value) }

    def packetsLength : Parser[Int] = """0[0,1]{15}""".r ^^ { s => bitsToInt(s.tail) }
    def packetsCount : Parser[Int] = """1[0,1]{11}""".r ^^ { s => bitsToInt(s.tail) }

    def numberOfPackets(nr: Int): Parser[Seq[Packet]] = repN(nr, packet)
    def packetsByCount: Parser[Seq[Packet]] = packetsCount >> { count => numberOfPackets(count)}
    def anyBit: Parser[Char] = """[0,1]""".r ^^ { _.head }
    // feed the parsed length to a new parser matches the full string and that parses it into packets
    def packetsByLength: Parser[Seq[Packet]] = packetsLength >> { length =>
      repN(length, anyBit) ^^ { packetString =>
        PacketParser.parse(rep(packet), packetString.mkString("")).get
      }
    }

    def operatorPacket: Parser[Operator] = version ~ operatorPacketId ~ (packetsByCount | packetsByLength) ^^ { case v ~ id ~ packets =>
      Operator(v, id, packets)
    }

    def packet: Parser[Packet] = literalPacket | operatorPacket
  }
}

object Day16Part1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day16.txt")
      bits = hexToBinary(sc.nextLine().trim())
      parsedPacket = PacketParser.parse(packet, bits) match {
        case PacketParser.Success(p, _) => p
        case PacketParser.NoSuccess(msg, _) => sys.error(msg)
      }
      versionNrs = allVersionNrs(parsedPacket)
      _ <- IO.delay(println("Solution: " + versionNrs.sum))
    } yield ExitCode.Success       // 960
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
      parsedPacket = PacketParser.parse(packet, bits) match {
        case PacketParser.Success(p, _) => p
        case PacketParser.NoSuccess(msg, _) => sys.error(msg)
      }
      result = evaluate(parsedPacket)
      _ <- IO.delay(println("Solution: " + result))
    } yield ExitCode.Success
  }
}


