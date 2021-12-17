package aoc2021

import aoc2021.Day16.hexToBinary

import scala.collection

object Test extends App {

  //val pattern = """target area: x=(-?d+)\.\.(-?d+), y=(-?d+)\.\.(-?d+)""".r
  val pattern = """target area: x=(-?\d+)""".r
  println(pattern.matches("target area: x=-1"))
}
