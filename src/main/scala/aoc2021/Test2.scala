package aoc2021

object Test2 extends App {
  val rots = Day19.matRotations(Pos3D(5,6,-4))
  println (rots.toSeq)
  println (rots.toSet.intersect(Set(Pos3D(-5,4,-6), Pos3D(4,6,5), Pos3D(-4,-6,5), Pos3D(-6,-4,-5))))
}
