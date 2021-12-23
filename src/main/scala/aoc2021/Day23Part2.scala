package aoc2021

import aoc2021.Day23Part1.inOwnRoom
import aoc2021.Grid.{printGrid, withDimensions}
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day23Part2 extends IOApp {

  // id to make them unique
  case class Amphipod(id: Int, kind: Char, position: Point, stepCost: Int, justMoved: Boolean) {
    val toState: AmphipodState = AmphipodState(kind, position, justMoved)
  }

  case class AmphipodState(kind: Char, position: Point, justMoved: Boolean)

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
    point.y >=2 && point.y <=5 && !inOwnRoom(pod, point)

  def inOwnRoom(pod: Amphipod, point: Point): Boolean =
    point.x == ownRoomX(pod) && point.y >=2 && point.y <= 5

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

  // returns Some if whole road is unblocked and the homw is free to enter, and pod is not in home already
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

  def isStupidMove(pod: Amphipod, newPos: Point, otherPods: Set[Amphipod], podsWithFreeWay: Set[Amphipod]): Boolean = {
    // if any of the pods has a free way to a safe home then any other move from another pod is stupid, if
    // this pod has higher kind
    podsWithFreeWay.exists(_.kind > pod.kind) ||
      (!podsWithFreeWay.contains(pod) && podsWithFreeWay.nonEmpty) ||
      // don't move anywhere else than towards own room if it is free and you're the highest pod with free way
      // (NOTE => previous rule guarantees this)
      (podsWithFreeWay.contains(pod) && newPos != nextPosToHome(pod, pod.position)) ||
      // don't move out of bottom of own home
      (inOwnRoom(pod, pod.position) && ownRoomSafe(pod, otherPods) && newPos.y < pod.position.y) ||
      // don't move anywhere else than to bottom of own room if next to it and it is free
      (inOwnRoom(pod, pod.position) && pod.position.y >= 2 && pod.position.y < 5 && ownRoomSafe(pod, otherPods) &&
        !(newPos.y == pod.position.y + 1 && newPos.x == pod.position.x)) ||
      // don't move out of own home when home is full with own pods
      (inOwnRoom(pod, pod.position) && {
        otherPods.filter(_.kind == pod.kind).forall { brother => inOwnRoom(brother, brother.position) }
      })  || {
      // move is upwards in (other) home, but other non-blocked candidate exists in other home that is lower or cheaper
      val lowestFirst = (!podsWithFreeWay.contains(pod) && (pod.position.y > 1 && newPos.y < pod.position.y && otherPods.exists { op =>
        op.position.y > 1 && op.position.x != pod.position.x && !inOwnRoom(op, op.position) &&
          // op can move all the way up
          (1 until op.position.y).forall { oy => otherPods.forall { opp => opp.position.x != op.position.x || opp.position.y != oy } } &&
          // and left or right in the hallway is a free space
          Seq(Point(op.position.x - 1, 1), Point(op.position.x + 1, 1)).exists { opp =>
            otherPods.forall { oppp => oppp.position.x != opp.x || oppp.position.y != opp.y }
          } &&
          (op.position.y > pod.position.y || (op.position.y == pod.position.y && op.kind < pod.kind))
      }))

      //      if (lowestFirst) {
      //        printState(grid, otherPods + pod)
      //        println(newPos)
      //      }

      lowestFirst
    }
  }

  def isValidMove(pod: Amphipod, newPos: Point, otherPods: Set[Amphipod], grid: Grid[Char]): Boolean = {
    grid.value(newPos) != '#' &&
      otherPods.forall(_.position != newPos) &&
      // don't stop on space outside room => this is a check for the -other- pods!
      otherPods.forall{ op => !isRightOutsideRoom(op.position)} &&
      // don't move into other room unless you were already there
      (!inOtherRoom(pod, newPos) || inOtherRoom(pod, pod.position)) &&
      // don't move into own room when other kind of pod is there
      (!inOwnRoom(pod, newPos) || ownRoomSafe(pod, otherPods) ) &&
      // stop moving rule. -> don't move when outside unless own place is free,
      // unless you were already moving (CHECK)
      (!isInHallWay(newPos) || isRightOutsideRoom(newPos) || pod.justMoved || isOwnRoomEntryFree(pod, otherPods))
  }

  def generateMoves(grid: Grid[Char], sol: Solution): Set[Solution] = {
    val Solution(score, /*steps,*/ pods) = sol
    // generate move (= a new list of pods), of one pod at a time
    //    println(s"---------------- Generating candidates for ---------------------")
    //    printState(grid, pods)
    //    println()

    val podsWithFreeWay: Set[Amphipod] = sol.pods.filter { p =>
      hasFreeWayToHome(sol.pods - p)(p)
    }

    val rawSolutions = pods.flatMap { pod  =>
      val otherPods = pods - pod
      val positions = grid.neighbours(pod.position)

      positions.filter{ p =>
        isValidMove(pod, p, otherPods, grid)
      }.filterNot { p => isStupidMove(pod, p, otherPods, podsWithFreeWay) }
        .map { p =>
          pod.copy(position = p, justMoved = true)
        }.map { newPod =>
        //val newStep = (newPod.kind, pod.position, newPod.position)
        val sol = Solution(score + newPod.stepCost, /*steps :+ newStep,*/ otherPods.map{op => op.copy(justMoved = false) } + newPod)
        sol
      }
    }

    val finalSolutions = if (rawSolutions.nonEmpty) {
      // prefer solutions that create the most freeHomes
      val withNrFreeHomes: Map[Int, Set[(Solution, Int)]] = rawSolutions.map { s =>
        s -> s.pods.count { p =>
          hasFreeWayToHome(s.pods - p)(p)
        }
      }.groupBy(_._2)

      withNrFreeHomes.toSeq.maxBy(_._1)._2.map(_._1)
    } else rawSolutions

    //    finalSolutions.foreach { sol =>
    //      println(s"---------------- ${sol.score} ---------------------")
    //      printState(grid, sol.pods)
    //    }
    finalSolutions
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

  def fillHomeFills(hf: mutable.Map[Char, Int], pods: Set[Amphipod]): Unit = {
    val homeByKind = pods.filter(p => inOwnRoom(p, p.position)).groupBy(_.kind).mapValues(_.size)

    Seq('A', 'B', 'C', 'D').foreach { kind =>
      if (hf.getOrElse(kind, 0) < homeByKind.getOrElse(kind, 0) ) {
        hf.update(kind, homeByKind(kind))
      }
    }
  }

  def kindAndHiger(k: Char): Seq[Char] = k to 'D'

  def findSolution(pods: Set[Amphipod], grid: Grid[Char]): Solution = {

    val queue = mutable.PriorityQueue[Solution]()(SolutionOrdering)
    queue.enqueue(Solution(0, /*Seq(),*/ pods))

    val visited = mutable.MultiSet[Set[AmphipodState]]()
    visited += pods.map(_.toState)

    var best = None : Option[Solution]
    val biggestHomeFillFound = mutable.Map[Char, Int]()
    fillHomeFills(biggestHomeFillFound, pods)

    while (best.isEmpty) {
      val next = queue.dequeue()
      //      println("-------------------------------------")
      //      printState(grid, next.pods)
      if (isFinalPos(next.pods)) {
        best = Some(next)
      } else {
        val newStates = generateMoves(grid, next).filterNot { m => visited.contains(m.pods.map(_.toState)) }
        visited.addAll(newStates.map(_.pods.map(_.toState)))

        val checkHomeFills = newStates.filterNot { s =>
          // for each kind;
          // count nr outside home;
          // if bigger than totalPods - maxFound then skip.
          val notHmeByKind = s.pods.filter(p => !inOwnRoom(p, p.position)).groupBy(_.kind).mapValues(_.size)

          notHmeByKind.exists { case (kind, nrNotAtHome) =>
            val bestAtHomeForKind = biggestHomeFillFound.getOrElse(kind, 0)
            nrNotAtHome > 2 - bestAtHomeForKind
          }
        }
        checkHomeFills.foreach { ch => fillHomeFills(biggestHomeFillFound, ch.pods) }

        queue.addAll(newStates)
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
      _ <- IO.delay(println("Solution: " + solution))
      //      _ <- IO.delay {
      //        finalPods.steps.foreach { s =>
      //          println (s._1 + " " + s._2 + " -> " + s._3)
      //        }
      //      }
    } yield ExitCode.Success
  }
}
