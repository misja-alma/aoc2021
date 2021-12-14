package aoc2021

import scala.collection

object Test extends App {

  var counter = 0
  def f(i: Int): Int = {
    counter += 1
    println (i)
    i + 1
  }

  val gen10 = iterate(0)(f).drop(10).head

  println ("gen10: " + gen10)
  println ("count " + counter)
}
