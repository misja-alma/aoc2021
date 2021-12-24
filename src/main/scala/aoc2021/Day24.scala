package aoc2021

import cats.effect.{ExitCode, IO, IOApp}

object Day24Part1 extends IOApp {

  sealed trait Operand {
    def value(state: State): Long
  }

  case class Value(x: Long) extends Operand {
    def value(state: State): Long = x
  }

  case class Variable(name: String) extends Operand {
    def value(state: State): Long = state.variables.getOrElse(name, 0)
  }

  sealed trait Command {
    def evaluate(state: State): State
  }

  case class Input(a: Variable) extends Command {
    def evaluate(state: State): State = {
      val newVars = state.variables + (a.name -> state.inputsLeft.head.toLong)
      State(newVars, state.inputsLeft.tail)
    }
  }

  case class Add(a: Variable, b: Operand) extends Command {
    def evaluate(state: State): State = {
      val result = a.value(state) + b.value(state)
      state.copy(variables = state.variables + (a.name -> result))
    }
  }

  case class Mul(a: Variable, b: Operand) extends Command {
    def evaluate(state: State): State = {
      val result = a.value(state) * b.value(state)
      state.copy(variables = state.variables + (a.name -> result))
    }
  }

  case class Div(a: Variable, b: Operand) extends Command {
    def evaluate(state: State): State = {
      val result = a.value(state) / b.value(state)
      state.copy(variables = state.variables + (a.name -> result))
    }
  }

  case class Mod(a: Variable, b: Operand) extends Command {
    def evaluate(state: State): State = {
      val result = a.value(state) % b.value(state)
      state.copy(variables = state.variables + (a.name -> result))
    }
  }

  case class Eql(a: Variable, b: Operand) extends Command {
    def evaluate(state: State): State = {
      val result = if (a.value(state) == b.value(state)) 1 else 0
      state.copy(variables = state.variables + (a.name -> result))
    }
  }


  def operand(s: String): Operand = {
    s.toIntOption match {
      case Some(x) => Value(x)
      case None => Variable(s)
    }
  }

  //inp a - Read an input value and write it to variable a.
  //add a b - Add the value of a to the value of b, then store the result in variable a.
  //mul a b - Multiply the value of a by the value of b, then store the result in variable a.
  //div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
  //mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
  //eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.
  def parseCommand(line: String): Command = {
    line match {
      case s"inp $a" => Input(Variable(a))
      case s"add $a $b" => Add(Variable(a), operand(b))
      case s"mul $a $b" => Mul(Variable(a), operand(b))
      case s"div $a $b" => Div(Variable(a), operand(b))
      case s"mod $a $b" => Mod(Variable(a), operand(b))
      case s"eql $a $b" => Eql(Variable(a), operand(b))
    }
  }

  case class State(variables: Map[String, Long], inputsLeft: Seq[Int])

  def evaluateInput(commands: Seq[Command], startState: State): State = {
    commands.foldLeft(startState) { case (state, cmd) =>
      cmd.evaluate(state)
    }
  }

  def findLargestValidNr(commands: Seq[Command]): Long = {
    val input = Seq(1,0,0,0,0,0,0,0,0,0,0,0,0,0)
    val vars = Map[String, Long]() + ("z" -> 0L)

    val startState = State(vars, input)
    val finalState = evaluateInput(commands, startState)
    println (finalState)
    finalState.variables("z")
  }

  case class InputFunction(par1: Int, par2: Int, divideZ: Int) {
    def calc(w: Int, z: Long): Long = {
      val x = z % 26 + par1
      val z2 = z / divideZ // means that for outcome to be possibly 0, previous z had to be < +/- 26 for last steps
      //    if (x == w) x = 0 else x = 1
      //    z * (25*x + 1) + x*(w + par2)
      //if (x == w) z else z * 26 + (w + par2)
      if (x == w) z2 else z2 * 26 + w + par2
    }
  }

  val functions = Seq[InputFunction] (
    InputFunction(10,15,1),
    InputFunction(12,8,1),
    InputFunction(15,2,1),
    InputFunction(-9,6,26),
    InputFunction(15,13,1),
    InputFunction(10,4,1),
    InputFunction(14,1,1),
    InputFunction(-5,9,26),
    InputFunction(14,5,1),
    InputFunction(-7,13,26),
    InputFunction(-12,9,26),
    InputFunction(-10,6,26),
    InputFunction(-1,2,26),
    InputFunction(-11,2,26)
  )

  def findHighestZero(functions: Seq[InputFunction], lastZ: Long): Option[Seq[Int]] = {
    val next = functions.head
    val rest = functions.tail
    if (next.divideZ == 1) {
      // just try all
      (9 to 1 by -1).toList.to(LazyList).flatMap { w =>
        val newZ = next.calc(w, lastZ)
        if (rest.isEmpty) {
          if (newZ == 0) Some(Seq(w)) else None
        } else {
          findHighestZero(rest, newZ).map { ws => w +: ws }
        }
      }.headOption
    } else {
      // take the highest possible based on lastZ
      if (rest.isEmpty) {
        if (lastZ >= 26) None else {
          val newW = lastZ % 26 + next.par1
          if (newW > 9 || newW < 1) None else Some(Seq(newW.toInt))
        }
      } else {
        // fix W so that we get the division
        val w = lastZ % 26 + next.par1
        if (w > 9 || w < 1) None else {
          val newZ = next.calc(w.toInt, lastZ)
          findHighestZero(rest, newZ).map { ws => w.toInt +: ws }
        }
      }
    }
  }

  def findLowestZero(functions: Seq[InputFunction], lastZ: Long): Option[Seq[Int]] = {
    val next = functions.head
    val rest = functions.tail
    if (next.divideZ == 1) {
      // just try all
      (1 to 9).toList.to(LazyList).flatMap { w =>
        val newZ = next.calc(w, lastZ)
        if (rest.isEmpty) {
          if (newZ == 0) Some(Seq(w)) else None
        } else {
          findLowestZero(rest, newZ).map { ws => w +: ws }
        }
      }.headOption
    } else {
      // take the highest possible based on lastZ
      if (rest.isEmpty) {
        if (lastZ >= 26) None else {
          val newW = lastZ % 26 + next.par1
          if (newW > 9 || newW < 1) None else Some(Seq(newW.toInt))
        }
      } else {
        // fix W so that we get the division
        val w = lastZ % 26 + next.par1
        if (w > 9 || w < 1) None else {
          val newZ = next.calc(w.toInt, lastZ)
          findLowestZero(rest, newZ).map { ws => w.toInt +: ws }
        }
      }
    }
  }

  def evaluateAll() = {
    val input = Seq(1,0,0,0,0,0,0,0,0,0,0,0,0,0)
    val z = 0L
    val (_, result) = functions.foldLeft((input, z)) { case ((is, zs), f) =>
      val newZ = f.calc(is.head, zs)
      (is.tail, newZ)
    }

    println (result)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/day24.txt")
      lines = scannerToLines(sc).filterNot(_.isBlank)
      _ <- IO.delay(println(findHighestZero(functions, 0).map(_.mkString(""))))
      _ <- IO.delay(println(findLowestZero(functions, 0).map(_.mkString(""))))
//      commands = lines.map(parseCommand)
//      solution = findLargestValidNr(commands)
//      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

