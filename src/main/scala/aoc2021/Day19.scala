package aoc2021

import aoc2021.Day19.{Pos3D, calculateScannerPositions, getUniqueBeacons, parseScanners}
import cats.effect.{ExitCode, IO, IOApp}

import scala.collection.mutable

object Day19 {
  case class Pos3D(x: Int, y: Int, z: Int) {
    def +(pos2: Pos3D): Pos3D = Pos3D(x + pos2.x, y + pos2.y, z + pos2.z)

    def -(pos2: Pos3D): Pos3D = Pos3D(x - pos2.x, y - pos2.y, z - pos2.z)
  }

  case class Scanner(nr: Int, scannedBeacons: Seq[Pos3D])

  // rot: permutation of x,y,z
  // (to) x = 0, y = 1, z = 2
  case class Permutation(xr: Int, yr: Int, zr: Int)

  // up/down: whether it is negated or not. 1 = normal, -1 is negated
  case class Mirroring(xup: Int, yup: Int, zup: Int)

  case class Rotation(perm: Permutation, mirr: Mirroring)

  def dim(pt: Pos3D, dim: Int): Int =
    dim match {
      case 0 => pt.x
      case 1 => pt.y
      case 2 => pt.z
    }

  def applyPermutation(pt: Pos3D, perm: Permutation): Pos3D = {
    Pos3D(dim(pt, perm.xr), dim(pt, perm.yr), dim(pt, perm.zr))
  }

  def matRotations(pt: Pos3D): Seq[Pos3D] = {
    iterate(pt) { p => Pos3D(p.z - p.y, p.x - p.z, p.y -p.z) }.take(24).toList
  }

  def applyRotation(pt: Pos3D, rotation: Rotation): Pos3D = {
    applyPermutation(applyMirrors(pt, rotation.mirr), rotation.perm)
  }

  // this returns 48 distinct rotations .. We should return only 24 but I don't know which
  def rotations(pt: Pos3D): Seq[(Rotation, Pos3D)] = {
    val forAllMirrors = for {
      mx <- Seq(-1,  1)
      my <- Seq(-1,  1)
      mz <- Seq(-1,  1)
    } yield {
      val mirroring = Mirroring(mx, my, mz)
      for {
        sx <- 0 to 2
        sy <- 0 to 2
        if sy != sx
        sz <- 0 to 2
        if sz != sy && sz != sx
      } yield {
        val permutation = Permutation(sx, sy, sz)
        val rotation = Rotation(permutation, mirroring)
        (rotation, applyRotation(pt, rotation))
      }
    }
    forAllMirrors.flatten
  }

  def parseScanners(lines: Seq[String]): Seq[Scanner] = {
    if (lines.isEmpty) Seq() else {
      val scLines = lines.takeWhile(_.nonEmpty)
      val scNr = scLines.head match {
        case s"--- scanner $nr ---" => nr.toInt
      }
      val scPositions = scLines.tail.map {
        case s"$x,$y,$z" => Pos3D(x.toInt, y.toInt, z.toInt)
      }
      Scanner(scNr, scPositions) +: parseScanners(lines.dropWhile(_.nonEmpty).dropWhile(_.isEmpty))
    }
  }

  // Try to match all scanners against scanner 0. For each match found, try to match
  // it with all unmatched scanners, etc.
  def calculateScannerPositions(scanners: Seq[Scanner]): Map[Scanner, (Pos3D, Rotation)] = {
    val result = mutable.Map[Scanner, (Pos3D, Rotation)]()
    val subject = scanners(0)
    val subjectPos = Pos3D(0,0,0)
    val subjectMirroring = Mirroring(1,1,1)
    val subjectPermutation = Permutation(0, 1, 2)
    result.put(subject, (subjectPos, Rotation(subjectPermutation, subjectMirroring)))
    val newFound = mutable.Queue(subject)

    while(newFound.nonEmpty) {
      val newSubject = newFound.dequeue()
      val (newSubjectPos, newSubjectRotation) = result(newSubject)
      for {
        sc2 <- scanners
        if !result.contains(sc2)
      } yield {
        val sc2Pos = findMatchingScanner(newSubject, newSubjectPos, newSubjectRotation, sc2)
        sc2Pos match {
          case Some((pos, rotation, matchingPts)) =>
            result += (sc2 -> (pos, rotation))
            newFound.enqueue(sc2)
          case None =>
            ()
        }
      }
    }

    result.toMap
  }

  def applyMirrors(pos: Pos3D, mirrors: Mirroring): Pos3D =
    Pos3D(pos.x * mirrors.xup, pos.y * mirrors.yup, pos.z * mirrors.zup)

  // try if the scanners match, by trying each beacon in scanner 1, translating it into a point, and then
  // for each point in sc2, for all possibly rotations, calculate the implicated translation for scanner 2 position
  // that this gives; apply this + the rotation to all points in scanner 2 and check if at least 12 match.
  def findMatchingScanner(scanner1: Scanner, scanner1Pos: Pos3D, scanner1Rotation: Rotation, scanner2: Scanner): Option[(Pos3D, Rotation, Set[Pos3D])] = {
    scanner1.scannedBeacons.flatMap { bDist1 =>

      val rotatedSc1Beacons = scanner1.scannedBeacons.map(b => applyRotation(b, scanner1Rotation) + scanner1Pos).toSet
      val bLoc1 = applyRotation(bDist1, scanner1Rotation) + scanner1Pos
      scanner2.scannedBeacons.flatMap { bDist2 =>
        for {
          rotated <- rotations(bDist2)
        } yield {
          val (rotation, rotatedDist) = rotated
          val deducedSc2Pos = bLoc1 - rotatedDist
          val deducedBeacons: Seq[Pos3D] = scanner2.scannedBeacons.map { b =>
            // convert using deduced pos
            val appliedRotation = applyRotation(b, rotation)
            deducedSc2Pos + appliedRotation
          }

          val matchingPositions = deducedBeacons.toSet.intersect(rotatedSc1Beacons)
          (deducedSc2Pos, rotation, matchingPositions)
        }
      }.find(_._3.size >= 12)
    }.headOption
  }

  def getUniqueBeacons(scanners: Seq[Scanner], positions: Map[Scanner, (Pos3D, Rotation)]): Set[Pos3D] = {
    scanners.flatMap { sc =>
      val (pos, rotation) = positions(sc)
      sc.scannedBeacons.map { bc =>
        applyRotation(bc, rotation) + pos
      }
    }.toSet
  }
}

object Day19Part1 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day19.txt")
      lines = scannerToLines(sc) 
      scanners = parseScanners(lines)
      scannerPositions = calculateScannerPositions(scanners)
      beacons = getUniqueBeacons(scanners, scannerPositions)
      _ <- IO.delay(println("Solution: " + beacons.size))
    } yield ExitCode.Success
  }
}

object Day19Part2 extends IOApp {

  def manhattanDistance(pt1: Pos3D, pt2: Pos3D): Int =
    Math.abs(pt1.x - pt2.x) + Math.abs(pt1.y - pt2.y) + Math.abs(pt1.z - pt2.z)

  def manhattanDistances(pts: Seq[Pos3D]): Seq[Int] = {
    pts.combinations(2).map { cs => manhattanDistance(cs(0), cs(1))}.toSeq
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day19.txt")
      lines = scannerToLines(sc)
      scanners = parseScanners(lines)
      scannerPositions = calculateScannerPositions(scanners)
      distances = manhattanDistances(scannerPositions.map(_._2._1).toSeq)
      _ <- IO.delay(println("Solution: " + distances.max))
    } yield ExitCode.Success
  }
}
