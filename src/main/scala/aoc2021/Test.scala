package aoc2021

object Test extends App {

  val grid = new Grid(Array(Array(1,2), Array(3,4)))
  println (grid.allPoints.head)

  println (grid.findPoint(_ == 3))
}
