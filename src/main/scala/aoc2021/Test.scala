package aoc2021

import aoc2021.Day16.hexToBinary
import aoc2021.Test.SimpleParser.{literalPacket, operatorPacket, operatorPacketId, packet, terminalSubPacket}

import scala.util.parsing.combinator.RegexParsers

object Test extends App {

  def bitsToInt(bits: String): Int =
    bits.foldLeft(0) { case (total, bit) => total * 2 + bit.asDigit }

  def hexToLong(hex: Seq[Int]): Long =
    hex.foldLeft(0L) { case (total, digit) => total * 16 + digit }


  sealed trait Packet {
    def version: Int
    def id: Int
  }

  case class Literal(version: Int, id: Int, value: Long) extends Packet
  case class Operator(version: Int, id: Int, subPackets: Seq[Packet]) extends Packet

  object SimpleParser extends RegexParsers {
    def version: Parser[Int] = """[0,1]{3}""".r ^^ { bitsToInt }

    def literalPacketId = "100"
    def operatorPacketId: Parser[Int] = """(000)|(001)|(010)|(011)|(101)|(110)|(111)""".r ^^ { bitsToInt }

    def subPacket : Parser[Int] = """1[0,1]{4}""".r ^^ { s => bitsToInt(s.tail) }
    def terminalSubPacket : Parser[Int] = """0[0,1]{4}""".r ^^ { s => bitsToInt(s.tail) }
    def packetValue: Parser[Long] = rep(subPacket) ~ terminalSubPacket ^^ { case subPackets ~ terminal => hexToLong(subPackets :+ terminal) }

    def literalPacket: Parser[Literal] = version ~ literalPacketId ~ packetValue ^^ { case version ~ id ~ value => Literal(version, 4, value) }

    def packetsLength : Parser[Int] = """0[0,1]{15}""".r ^^ { s => bitsToInt(s.tail) }
    def packetsCount : Parser[Int] = """1[0,1]{11}""".r ^^ { s => bitsToInt(s.tail) }

    def numberOfPackets(nr: Int): Parser[Seq[Packet]] = repN(nr, literalPacket | operatorPacket)
    def packetsByCount: Parser[Seq[Packet]] = packetsCount >> { count => numberOfPackets(count)}
    def anyBit: Parser[Char] = """[0,1]""".r ^^ { _.head }
    // feed the parsed length to a new parser matches the full string and that parses it into packets
    def packetsByLength: Parser[Seq[Packet]] = packetsLength >> { length =>
      repN(length, anyBit) ^^ { packetString =>
        SimpleParser.parse(rep(literalPacket | operatorPacket), packetString.mkString("")).get
      }
    }

    def operatorPacket: Parser[Operator] = version ~ operatorPacketId ~ (packetsByCount | packetsByLength) ^^ { case v ~ id ~ packets =>
      Operator(v, id, packets)
    }

    def packet = literalPacket | operatorPacket
  }

  val parsed = SimpleParser.parse(packet, hexToBinary("8A004A801A8002F478"))
  println (parsed)

}             
