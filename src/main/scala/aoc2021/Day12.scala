package aoc2021

import aoc2021.Day12.{isUppercase, mergeEdges, toEdges}
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day12 {
  def toEdges(line: String): Seq[(String, String)] = {
    val Array(to, from) = line.split("-")
    Seq(to -> from ,from -> to)
  }

  def mergeEdges(edges: Seq[(String, String)]): Map[String, mutable.Set[String]] = {
    val mm = new mutable.HashMap[String, mutable.Set[String]] with mutable.MultiMap[String, String]
    edges.foreach { case (to, from) =>
      mm.addBinding(to, from)
    }
    mm.toMap
  }

  def isUppercase(str: String): Boolean = str.forall(_.isUpper)
}

object Day12Part1 extends IOApp {

  def findAllPaths(adjList: Map[String, mutable.Set[String]]): Set[Seq[String]] = {
    val result = mutable.Set[Seq[String]]()
    val queue = mutable.Queue(Seq("start"))
    while(queue.nonEmpty) {
      val path = queue.dequeue()
      if (path.head == "end") result += path
      else {
        val neighbours = adjList.getOrElse(path.head, Seq())
        neighbours.foreach { nb =>
          if (isUppercase(nb) || !path.contains(nb)) queue.enqueue(nb +: path)
        }
      }
    }
    result.toSet
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day12.txt")
      lines = scannerToLines(sc)
      // build adjacency list
      biDirAdjList = mergeEdges(lines.flatMap(toEdges))
      // bfs, we don't need a visited set because we check per path
      // keep a seq of visited nodes per entry
      allPaths = findAllPaths(biDirAdjList)
      _ <- IO.delay(println("Solution: " + allPaths.size))
    } yield ExitCode.Success
  }
}

object Day12Part2 extends IOApp {

  case class Path(head: String, lowcaseNodes: Map[String, Int])
  
  def findAllPaths(adjList: Map[String, mutable.Set[String]]): Long = {
    var result = 0L
    val queue = mutable.Queue(Path("start", Map()))
    while(queue.nonEmpty) {
      val path = queue.dequeue()
      if (path.head == "end") result += 1
      else {
        val neighbours = adjList.getOrElse(path.head, Seq())
        neighbours.foreach { nb =>
          if (nb != "start" && (isUppercase(nb) || !path.lowcaseNodes.contains(nb) || path.lowcaseNodes.values.forall(_ < 2))) {
            val newLowcaseNodes =
              if (isUppercase(nb)) path.lowcaseNodes else
                if (path.lowcaseNodes.contains(nb)) path.lowcaseNodes + (nb -> 2) else path.lowcaseNodes + (nb -> 1)
            val newPath = Path(nb, newLowcaseNodes)
            queue.enqueue(newPath)
          }
        }
      }
    }
    result
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day12.txt")
      lines = scannerToLines(sc)
      // build adjacency list
      biDirAdjList = mergeEdges(lines.flatMap(toEdges))
      solution = findAllPaths(biDirAdjList)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}