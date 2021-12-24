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
    val input = Seq(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    val vars = Map[String, Long]() + ("z" -> 7L)

    val startState = State(vars, input)
    val finalState = evaluateInput(commands, startState)
    println (finalState)
    finalState.variables("z")
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      sc <- scannerFromResource("/test.txt")
      lines = scannerToLines(sc).filterNot(_.isBlank)
      commands = lines.map(parseCommand)
      solution = findLargestValidNr(commands)
      _ <- IO.delay(println("Solution: " + solution))
    } yield ExitCode.Success
  }
}

object TestFunc extends  App {


  def partFunc2(par1: Long, par2: Long)(w: Long, z: Long): Long = {
    val x = z % 26 + par1
    val z2 = z / 26
    //    if (x == w) x = 0 else x = 1
    //    z * (25*x + 1) + x*(w + par2)
    if (x == w) z2 else w + par2
  }

  def partFunc(par1: Long, par2: Long, par3: Long)(w: Long, z: Long): Long = {
    val x = z % 26 + par1
    val z2 = z / par3   // means that for outcome to be possibly 0, previous z had to be < +/- 26 for last steps
//    if (x == w) x = 0 else x = 1
//    z * (25*x + 1) + x*(w + par2)
    //if (x == w) z else z * 26 + (w + par2)
    if (x == w) z2 else  z* 26 + w + par2    // modulo 26 is normally removed in next step but counts for par3 = 1
  }

  println (partFunc(-11, 2, 26)(9, 20))
}
