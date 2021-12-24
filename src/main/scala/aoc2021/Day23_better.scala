package aoc2021

import aoc2021.Grid.{printGrid, withDimensions}
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day23Part1_better extends IOApp {

  // id to make them unique
  case class Amphipod(id: Int, kind: Char, position: Point, stepCost: Int, justMoved: Boolean)

  //case class Solution(score: Long, steps: Seq[(Char, Point, Point)], pods: Set[Amphipod])
  case class Solution(score: Long,  pods: Set[Amphipod])

  def scanAmphipods(grid: Grid[Char]): Set[Amphipod] = {
    var podCount = 0
    def nextId: Int = {
      podCount += 1
      podCount
    }

    grid.allPoints.flatMap { p =>
      grid.value(p) match {
        case 'A' => Some(Amphipod(nextId, 'A', p, 1, false))
        case 'B' => Some(Amphipod(nextId, 'B', p, 10, false))
        case 'C' => Some(Amphipod(nextId, 'C', p, 100, false))
        case 'D' => Some(Amphipod(nextId, 'D', p, 1000, false))
        case _ => None
      }
    }.toSet
  }

  def isFinalPos(pods: Set[Amphipod]): Boolean = {
    pods.forall { p => inOwnRoom(p, p.position) }
  }

  def ownRoomX(pod: Amphipod): Int = pod.kind match {
    case 'A' => 3
    case 'B' => 5
    case 'C' => 7
    case 'D' => 9
  }

  def isRightOutsideRoom(point: Point): Boolean = point.y == 1 &&
    (point.x == 3 || point.x == 5 || point.x == 7 || point.x == 9)

  def isInHallWay(point: Point): Boolean = point.y == 1 && point.x > 0 && point.x < 12

  def inOtherRoom(pod: Amphipod, point: Point): Boolean =
    point.y >=2 && point.y <=3 && !inOwnRoom(pod, point)

  def inOwnRoom(pod: Amphipod, point: Point): Boolean =
    point.x == ownRoomX(pod) && point.y >=2 && point.y <=3

  def ownRoomSafe(pod: Amphipod, otherPods: Set[Amphipod]): Boolean = {
    otherPods.filterNot(_.kind == pod.kind).forall {  op =>
      op.position.y < 2 || op.position.x != ownRoomX(pod)
    }
  }

  def isOwnRoomEntryFree(pod: Amphipod, otherPods: Set[Amphipod]): Boolean = {
    val entryPos = Point(ownRoomX(pod), 2)
    otherPods.forall { op =>
      op.position != entryPos
    }
  }

  def nextPosToHome(pod: Amphipod, point: Point): Point = {
    val homeX = ownRoomX(pod)
    if (point.x == homeX) {
      point.copy(y = point.y + 1)
    } else {
      if (point.y > 1) point.copy(y = point.y - 1)
      else {
        point.copy(x = point.x + (homeX - point.x).sign)
      }
    }
  }

  // returns Some if whole road is unblocked and the home is free to enter, and pod is not in home already
  def hasFreeWayToHome(otherPods: Set[Amphipod])(pod: Amphipod): Boolean = {
    !inOwnRoom(pod, pod.position) &&
      isOwnRoomEntryFree(pod, otherPods) && ownRoomSafe(pod, otherPods) && {
      var free = true
      var freePos = pod.position
      while(free && !inOwnRoom(pod, freePos)) {
        freePos = nextPosToHome(pod, freePos)
        free = otherPods.forall(_.position != freePos)
      }
      free
    }
  }


  def isValidMove(pod: Amphipod, newPos: Point, otherPods: Set[Amphipod], grid: Grid[Char]): Boolean = {
    grid.value(newPos) != '#' &&
      otherPods.forall(_.position != newPos) &&
      // don't move into other room unless you were already there
      (!inOtherRoom(pod, newPos) || inOtherRoom(pod, pod.position)) &&
      // don't move into own room when other kind of pod is there
      (!inOwnRoom(pod, newPos) || ownRoomSafe(pod, otherPods) ) &&
      // stop moving rule. -> don't move when outside unless own place is free,
      // unless you were already moving (CHECK)
      (!isInHallWay(newPos) || isRightOutsideRoom(newPos) || pod.justMoved || isOwnRoomEntryFree(pod, otherPods))
  }

  def isValidEndpoint(pod: Amphipod, newPos: Point, otherPods: Set[Amphipod], grid: Grid[Char]): Boolean = {
    grid.value(newPos) != '#' &&
      otherPods.forall(_.position != newPos) &&
      // don't stop on space outside room => this is a check for the -other- pods!
      otherPods.forall{ op => !isRightOutsideRoom(op.position)}
  }

  def isIntermediateEndpoint(pos: Point): Boolean = {
    pos.y == 1 && (pos.x % 2 == 0)
  }

  def allEndPositions(grid: Grid[Char], pod: Amphipod, otherPods: Set[Amphipod]): Set[(Int, Amphipod)] = {
    // dfs; take all neighbours that are not visited yet
    // take only valid ones => NOTE: we need a subset of isValid!
    // update whole pod for each move!
    // also update cost
    // collect endpoints along the way.
    // endpoints are:
    // - last point before block (eg in own home or both corners)
    // - on top of the walls between homes
    // so before recursing, we need to check if there are any candidates at all;
    // if yes then we have an endpoint. But it might be invalid; check some rules in isValid
    val visited = mutable.Set[Point]()
    val endPoints = mutable.Set[(Int, Amphipod)]()
    val stack = mutable.Stack[(Int, Amphipod)]()
    stack.push((0, pod))
    visited.add(pod.position)
    while (stack.nonEmpty) {
      val (cost, next) = stack.pop()
      val nextOnes = grid.neighbours(next.position)
        .filter { p =>  isValidMove(next, p, otherPods, grid) }
        .map { p => (cost + next.stepCost, next.copy(position = p, justMoved = true )) }
        .filterNot { p => visited(p._2.position) }
      if (nextOnes.isEmpty) {
        if (isValidEndpoint(next, next.position, otherPods, grid)) {
          endPoints.add((cost, next))
        }
      } else {
        endPoints.addAll(nextOnes.filter { case (_, p) => isIntermediateEndpoint(p.position) && isValidEndpoint(p, p.position, otherPods, grid)})
        stack.addAll(nextOnes)
        visited.addAll(nextOnes.map(_._2.position))
      }
    }

    endPoints.toSet
  }

  def generateMoves(grid: Grid[Char], sol: Solution): Set[Solution] = {
    val Solution(score, /*steps,*/ pods) = sol
    // generate move (= a new list of pods), of one pod at a time
//    println(s"---------------- Generating candidates for ---------------------")
//    printState(grid, pods)
//    println()

    pods.flatMap { pod  =>
      val otherPods = pods - pod
      val positions = allEndPositions(grid, pod, otherPods)
      val endInOwnRoom = positions.filter { case (_, p) => inOwnRoom(p, p.position) }
      // prefer moves that end in own room except when we started there already
      val positionsToUse = if (endInOwnRoom.nonEmpty && !inOwnRoom(pod, pod.position)) endInOwnRoom else positions

      positionsToUse.map { case (cost, newPod) =>
           val sol = Solution(score + cost, otherPods.map{op => op.copy(justMoved = false) } + newPod)
           sol
        }
    }
  }

  object SolutionOrdering extends Ordering[Solution] {
    def compare(p1: Solution, p2: Solution) = {
      p2.score.compareTo(p1.score)
    }  // reverse ordering for Scala's pq; it will put the largest in the head but we want the shortest path
  }

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

  def findSolution(pods: Set[Amphipod], grid: Grid[Char]): Solution = {

    val queue = mutable.PriorityQueue[Solution]()(SolutionOrdering)
    queue.enqueue(Solution(0, /*Seq(),*/ pods))

    val visited = mutable.MultiSet[Set[Amphipod]]()
    visited += pods

    var best = None : Option[Solution]

    while (best.isEmpty) {
      val next = queue.dequeue()
      println("----------------- Candidate --------------------")
      printState(grid, next.pods)
      if (isFinalPos(next.pods)) {
        best = Some(next)
      } else {
        val newStates = generateMoves(grid, next).filterNot { m => visited.contains(m.pods) }
        visited.addAll(newStates.map(_.pods))

        newStates.foreach { sol =>
          println(s"---------------- ${sol.score} ---------------------")
          printState(grid, sol.pods)
        }

        queue.addAll(newStates)
      }
    }

    best.get
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/test2.txt")
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
