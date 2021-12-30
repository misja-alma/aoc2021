package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day23Part2 extends IOApp {
  val deepestHomePt = 5
  val printTrace = false

  // id to make them unique
  case class Amphipod(id: Int, kind: Char, position: Point, stepCost: Int) {

    override def equals(obj: Any): Boolean = {
      obj.isInstanceOf[Amphipod] && {
        val other = obj.asInstanceOf[Amphipod]
        other.id == id && other.position == position
      }
    }

    override def hashCode(): Int = cachedHash

    private lazy val cachedHash = id.hashCode() * 31 + position.hashCode()
  }

  case class Solution(score: Long, steps: Seq[(Char, Point, Point)], pods: Set[Amphipod]) {
    lazy val totalCostToHome: Int = pods.map { p =>
      if (inOwnRoom(p)) 0 else {
        val homePt = Point(ownRoomX(p), 2)
        distance(p.position, homePt) * p.stepCost
      }
    }.sum
  }
  //case class Solution(score: Long,  pods: Set[Amphipod])

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

  def distance(pFrom: Point, pTo: Point): Int = {
    if (pTo.y >= 2) {
      // to some home
      if (pFrom.y >= 2) {
        // from some home
        if (pFrom.x == pTo.x) {
          0 // don't count moves inside any home
        } else {
          // other home
          pFrom.y - 1 + Point.manhattanDistance(pFrom.copy(y = 1), pTo)
        }
      } else {
        // from hallway
        Point.manhattanDistance(pFrom, pTo)
      }
    } else {
      // to hallway from home
      Point.manhattanDistance(pFrom, pTo)
    }
  }

  def inOwnRoom(pod: Amphipod): Boolean =
    pod.position.x == ownRoomX(pod) && pod.position.y >= 2 && pod.position.y <= deepestHomePt

  def ownRoomSafe(pod: Amphipod, otherPods: Set[Amphipod]): Boolean = {
    otherPods.filterNot(_.kind == pod.kind).forall { op =>
      op.position.y < 2 || op.position.x != ownRoomX(pod)
    }
  }

  // Requires that pod is in some home
  def reachableHallways(pod: Amphipod, otherPods: Map[Point, Amphipod]): Set[(Int, Point)] = {
    if (pod.position.y == 1) Set() else {
      // check if road free until entry point
      val freeUntilEntry = (pod.position.y - 1 to 1 by -1).forall { y => !otherPods.contains(Point(pod.position.x, y)) }
      if (!freeUntilEntry) Set() else {
      // list all free endpoints to the left + all to the right
        // find first occupied to left; take free points until there
        val firstOccLeft = ((pod.position.x - 1) to 1 by -1).find { x =>
          otherPods.contains(Point(x, 1))
        }.getOrElse(0)
        val toLeft = ((pod.position.x - 1) until firstOccLeft by -1).flatMap { x =>
          if (x % 2 == 0 || x == 1) {
            val newCost = (pod.position.x - x) * pod.stepCost
            Some(Point(x, 1))
          } else None
        }

        // same for right
        val firstOccRight = ((pod.position.x + 1) to 11).find { x =>
          otherPods.contains(Point(x, 1))
        }.getOrElse(12)
        val toRight = ((pod.position.x + 1) until firstOccRight).flatMap { x =>
          if (x % 2 == 0 || x == 11) {
            val newCost = (x - pod.position.x) * pod.stepCost
            Some(Point(x, 1))
          } else None
        }

        toLeft ++ toRight
      }.toSet.map { dest: Point => distance(pod.position, dest) * pod.stepCost -> dest }
    }
  }

  // Returns None if pod is in its own home already
  def deepestReachableHomePoint(pod: Amphipod, otherPods: Map[Point, Amphipod]): Option[(Int, Point)] = {
    if (!ownRoomSafe(pod, otherPods.values.toSet) || inOwnRoom(pod)) None else {
      // pod in some homepoint (not own); check if points to hallway are free
      val maybeCostToHall = if (pod.position.y >= 2) {
        val free = (pod.position.y - 1 to 1 by -1).forall { y => !otherPods.contains(Point(pod.position.x, y)) }
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

  def generateMoves(sol: Solution): Set[Solution] = {
    val Solution(score, steps, pods) = sol

    val allPods = pods.map { p => p.position -> p }.toMap
    //  move only pods that are not in their homes  yet
    pods.filterNot { p => safeAtHome(p, allPods - p.position) }.flatMap { pod =>
      val otherPods = (pods - pod).map { p => p.position -> p }.toMap
      val positions = allEndPositions(pod, otherPods)

      positions.map { case (cost, newPos) =>
        val newPod = pod.copy(position = newPos)
        val step = (pod.kind, pod.position, newPos)
        Solution(score + cost, step +: steps, otherPods.values.toSet + newPod)
      }
    }
  }

  // reverse ordering for Scala's pq; it will put the largest in the head but we want the shortest path
  private val solutionOrdering = Ordering.by[Solution, Long] { s => -s.score - s.totalCostToHome }

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

  var stepCount = 0

  def findSolution(pods: Set[Amphipod], grid: Grid[Char]): Solution = {

    val queue = mutable.PriorityQueue[Solution]()(solutionOrdering)
    queue.enqueue(Solution(0, Seq(), pods))

    val visited = mutable.MultiSet[Set[Amphipod]]()

    var best = None: Option[Solution]

    while (best.isEmpty) {
      val next: Solution = queue.dequeue()
      if (!visited.contains(next.pods)) {
        visited += next.pods
        stepCount += 1

        if (printTrace) {
          println("----------------- Candidate --------------------")
          printState(grid, next.pods)
        }

        if (isFinalPos(next.pods)) {
          best = Some(next)
        } else {
          val newStates = generateMoves(next).filterNot { m => visited.contains(m.pods) }

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
      finalPods = findSolution(amphipods, grid)
      solution = finalPods.score
      _ <- IO.delay(println("Solution: " + solution))
      _ <- IO.delay {
        printState(grid, amphipods)
        println (s"----------- Total Steps: $stepCount --------------")
        var pods = amphipods
        var count = 0
        
        finalPods.steps.reverse.foreach { case (k, p1, p2) =>
          count = count + 1
          println("Step " + count + ": " + k + " " + p1 + " -> " + p2)
          val oldPod = pods.find { p => p.kind == k && p.position == p1 }.get
          val newPod = oldPod.copy(position = p2)
          pods = pods - oldPod + newPod
          printState(grid, pods)
        }
      }
    } yield ExitCode.Success
  }
}

// 222963 steps
// 219059 steps with cost to home included
