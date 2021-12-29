package aoc2021

import aoc2021.Grid.{printGrid, withDimensions}
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day23Part2 extends IOApp {
  val deepestHomePt = 5
  val printTrace = false

  // id to make them unique
  case class Amphipod(id: Int, kind: Char, position: Point, stepCost: Int)

  //case class Solution(score: Long, steps: Seq[(Char, Point, Point)], pods: Set[Amphipod])
  case class Solution(score: Long,  pods: Set[Amphipod], nrSafeAtHome: Int)

  def scanAmphipods(grid: Grid[Char]): Set[Amphipod] = {
    var podCount = 0
    def nextId: Int = {
      podCount += 1
      podCount
    }

    grid.allPoints.flatMap { p =>
      grid.value(p) match {
        case 'A' => Some(Amphipod(nextId, 'A', p, 1))
        case 'B' => Some(Amphipod(nextId, 'B', p, 10))
        case 'C' => Some(Amphipod(nextId, 'C', p, 100))
        case 'D' => Some(Amphipod(nextId, 'D', p, 1000))
        case _ => None
      }
    }.toSet
  }

  def isFinalPos(pods: Set[Amphipod]): Boolean = {
    pods.forall { p => inOwnRoom(p) }
  }

  def ownRoomX(pod: Amphipod): Int = pod.kind match {
    case 'A' => 3
    case 'B' => 5
    case 'C' => 7
    case 'D' => 9
  }

  def inOwnRoom(pod: Amphipod): Boolean =
    pod.position.x == ownRoomX(pod) && pod.position.y >=2 && pod.position.y <= deepestHomePt

  def ownRoomSafe(pod: Amphipod, otherPods: Set[Amphipod]): Boolean = {
    otherPods.filterNot(_.kind == pod.kind).forall {  op =>
      op.position.y < 2 || op.position.x != ownRoomX(pod)
    }
  }

  // Requires that pod is in some home
  def reachableHallways(pod: Amphipod, otherPods: Map[Point, Amphipod]): Set[(Int, Point)] = {
    if (pod.position.y == 1) Set() else {
      // check if road free until entry point
      val freeUntilEntry = (pod.position.y - 1 to 1 by -1).forall { y => !otherPods.contains(Point(pod.position.x, y)) }
      val maybeCostToHall = if (freeUntilEntry) {
        Some((pod.position.y - 1) * pod.stepCost)
      } else None
      // list all free endpoints to the left + all to the right
      maybeCostToHall.toSeq.flatMap { cost =>
        // find first occupied to left; take free points until there
        val firstOccLeft = ((pod.position.x - 1) to 1 by -1).find { x =>
          otherPods.contains(Point(x, 1))
        }.getOrElse(0)
        val toLeft = ((pod.position.x - 1) until firstOccLeft by -1).flatMap { x =>
          if (x % 2 == 0 || x == 1) {
            val newCost = (pod.position.x - x) * pod.stepCost
            Some((cost + newCost) -> Point(x, 1))
          } else None
        }

        // same for right
        val firstOccRight = ((pod.position.x + 1) to 11).find { x =>
          otherPods.contains(Point(x, 1))
        }.getOrElse(12)
        val toRight = ((pod.position.x + 1) until firstOccRight).flatMap { x =>
          if (x % 2 == 0 || x == 11) {
            val newCost = (x - pod.position.x) * pod.stepCost
            Some((cost + newCost) -> Point(x, 1))
          } else None
        }

        toLeft ++ toRight
      }.toSet
    }
  }

  // Returns None if pod is in its own home already
  def deepestReachableHomePoint(pod: Amphipod, otherPods: Map[Point, Amphipod]): Option[(Int, Point)] = {
    if (!ownRoomSafe(pod, otherPods.values.toSet) || inOwnRoom(pod)) None else {
      // pod in some homepoint (not own); check if points to hallway are free
      val maybeCostToHall = if (pod.position.y >= 2) {
        val free = (pod.position.y - 1 to 1 by -1).forall { y => !otherPods.contains(Point(pod.position.x, y))}
        if (free) {
          Some((pod.position.y - 1) * pod.stepCost)
        } else None
      } else Some(0)
      // check from hallway; check if all points to entry point are free
      val maybeCostToHome: Option[Int] = maybeCostToHall flatMap { cost =>

        val free = (pod.position.x to ownRoomX(pod) by (ownRoomX(pod) - pod.position.x).sign).forall { x =>
          !otherPods.contains(Point(x, 1))
        }
        if (free) {
          Some(cost + Math.abs(pod.position.x - ownRoomX(pod)) * pod.stepCost)
        } else {
          None
        }
      }
      // give deepest free home point, if any
      maybeCostToHome.flatMap { cost =>
        val destPoint = (2 to deepestHomePt).find { y => otherPods.contains(Point(ownRoomX(pod), y)) } match {
          case None => Some(Point(ownRoomX(pod), deepestHomePt))
          case Some(occupiedPoint) => if (occupiedPoint > 2) Some(Point(ownRoomX(pod), occupiedPoint - 1)) else None
        }
        destPoint.map { pt =>
          val newCost = (pt.y - 1) * pod.stepCost
          (cost + newCost) -> pt
        }
      }
    }
  }

  def safeAtHome(pod: Amphipod, otherPods: Map[Point, Amphipod]): Boolean =
    inOwnRoom(pod) && (pod.position.y + 1 to deepestHomePt).forall { y =>
      val pos = Point(pod.position.x, y)
      otherPods.get(pos) match {
        case Some(pd) => pd.kind == pod.kind
        case None => true
      }
    }

  def allEndPositions(pod: Amphipod, otherPods: Map[Point, Amphipod]): Set[(Int, Point)] = {
    // when outside, their can be only 1 end position:
    // the own home, the furthest position, if it is reachable and allowed
    // when inside, all valid hallway positions are possible when they are reachable but prefer going home if possible
    // but ofc not when already in own home with only own kind below
    if (safeAtHome(pod, otherPods)) {
      Set()
    } else {
      deepestReachableHomePoint(pod, otherPods) match {
        case Some(pt) => Set(pt) // always prefer going straight home
        case None => if (pod.position.y == 1) Set() else reachableHallways(pod, otherPods)
      }
    }
  }

  def generateMoves(grid: Grid[Char], sol: Solution): Set[Solution] = {
    val Solution(score, /*steps,*/ pods, _) = sol

    val allPods = pods.map { p => p.position -> p }.toMap
    //  move only pods that are not in their homes  yet
    val allResults = pods.filterNot{ p => safeAtHome(p, allPods - p.position) }.flatMap { pod  =>
      val otherPods = (pods - pod).map { p => p.position -> p }.toMap
      val positions = allEndPositions(pod, otherPods)

      positions.map { case (cost, newPos) =>
         val newPod = pod.copy(position = newPos)
         val nrAtHome = sol.nrSafeAtHome + (if (safeAtHome(newPod, otherPods)) 1 else 0)
         Solution(score + cost, otherPods.values.toSet + newPod, nrAtHome)
      }
    }

    // prefer moves that end in own room
    val highestRoomCounts = if (allResults.isEmpty) allResults else allResults.groupBy(_.nrSafeAtHome).maxBy(_._1)._2
    highestRoomCounts
  }

  // reverse ordering for Scala's pq; it will put the largest in the head but we want the shortest path
  val solutionOrdering = Ordering.by[Solution, Long]( -_.score )
//  object SolutionOrdering extends Ordering[Solution] {
//    def compare(p1: Solution, p2: Solution) = {
//        p2.score.compareTo(p1.score)
//    }
//  }

  def printState(grid: Grid[Char], pods: Set[Amphipod]): Unit = {
    (0 until grid.height).foreach { row =>
      (0 until grid.width).foreach { col =>
        val pos = Point(col, row)
        pods.find(_.position == pos) match {
          case Some(pod) => print(pod.kind)
          case None => if (grid.value(pos) == '#') print('#') else print('.')
        }
      }
      println()
    }
  }

  def countSafeAtHome(pods: Set[Amphipod]): Int = {
    val allPods = pods.map { p => p.position -> p }.toMap
    pods.count { p => safeAtHome(p, allPods - p.position)}
  }

  def findSolution(pods: Set[Amphipod], grid: Grid[Char]): Solution = {

    val queue = mutable.PriorityQueue[Solution]()(solutionOrdering)
    queue.enqueue(Solution(0, /*Seq(),*/ pods, countSafeAtHome(pods)))

    val visited = mutable.MultiSet[Set[Amphipod]]()

    var best = None : Option[Solution]

    while (best.isEmpty) {
      val next: Solution = queue.dequeue()
      if (!visited.contains(next.pods)) {
        visited += next.pods

        if (printTrace) {
          println("----------------- Candidate --------------------")
          printState(grid, next.pods)
        }

        if (isFinalPos(next.pods)) {
          best = Some(next)
        } else {
          val newStates = generateMoves(grid, next).filterNot { m => visited.contains(m.pods) }
          //visited.addAll(newStates.map(_.pods))

          if (printTrace) {
            newStates.foreach { sol =>
              println(s"---------------- ${sol.score} ---------------------")
              printState(grid, sol.pods)
            }
          }

          queue.addAll(newStates)
        }
      }
    }

    best.get
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day23_2.txt")
      lines = scannerToLines(sc)
      grid = new Grid(lines.map(line => line.padTo(13, ' ').toArray).toArray)
      amphipods = scanAmphipods(grid)
      _ <- IO.delay(printGrid(grid))
      finalPods = findSolution(amphipods, grid)
      solution = finalPods.score
      _ <- IO.delay(println("Solution: " + solution))    // 15160
//      _ <- IO.delay {
//        finalPods.steps.foreach { s =>
//          println (s._1 + " " + s._2 + " -> " + s._3)
//        }
//      }
    } yield ExitCode.Success
  }
}
