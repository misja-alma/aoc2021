package aoc2021

import scala.collection._

object Test extends App {

  val ms = MultiSet[String]()
  val ms2: immutable.MultiSet[String] = ms + "foo"
  println(ms2.get("foo"))
}
