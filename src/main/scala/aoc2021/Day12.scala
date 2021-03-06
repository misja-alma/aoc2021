package aoc2021

import aoc2021.Day12.{isUppercase, mergeEdges, toBidirectionalEdges}
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.immutable.MultiSet
import scala.collection.mutable

object Day12 {
  def toBidirectionalEdges(line: String): Seq[(String, String)] = line match {
    case s"$from-$to" => Seq((from, to))
    case _ => sys.error("Can't parse line: " + line)
  }

  def mergeEdges(edges: Seq[(String, String)]): Map[String, Set[String]] =
    edges
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap

  def isUppercase(str: String): Boolean = str.forall(_.isUpper)
}

object Day12Part1 extends IOApp {

  def findAllPaths(adjList: Map[String, Set[String]]): Set[Seq[String]] = {
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
      biDirAdjList = mergeEdges(lines.flatMap(toBidirectionalEdges))
      // bfs, we don't need a visited set because we check per path
      // keep a seq of visited nodes per entry
      allPaths = findAllPaths(biDirAdjList)
      _ <- IO.delay(println("Solution: " + allPaths.size))
    } yield ExitCode.Success
  }
}

object Day12Part2 extends IOApp {

  case class Path(head: String, lowcaseNodes: MultiSet[String])

  def findAllPaths(adjList: Map[String, Set[String]]): Long = {
    var result = 0L
    val queue = mutable.Queue(Path("start", MultiSet()))
    while(queue.nonEmpty) {
      val path = queue.dequeue()
      if (path.head == "end") result += 1
      else {
        val neighbours = adjList.getOrElse(path.head, Seq())
        neighbours.foreach { nb =>
          if (nb != "start" && (isUppercase(nb) || !path.lowcaseNodes.contains(nb) || path.lowcaseNodes.occurrences.forall(_._2 < 2))) {
            val newLowcaseNodes =
              if (isUppercase(nb)) path.lowcaseNodes else path.lowcaseNodes + nb
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
      biDirAdjList = mergeEdges(lines.flatMap(toBidirectionalEdges))
      solution = findAllPaths(biDirAdjList)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}