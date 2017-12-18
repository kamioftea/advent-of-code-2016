import Day18.Instruction
import akka.typed.{ActorRef, ActorSystem, Behavior}
import akka.typed.scaladsl.Actor

import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global

object Day18 {

  sealed trait Value {
    def getValue(registers: Map[Char, Long]): Long
  }

  case class Ref(register: Char) extends Value {
    override def getValue(registers: Map[Char, Long]): Long = registers.getOrElse(register, 0)
  }

  case class Literal(value: Long) extends Value {
    override def getValue(registers: Map[Char, Long]): Long = value
  }

  case object EmptyValue extends Value {
    override def getValue(registers: Map[Char, Long]): Long = throw new RuntimeException("Trying to get empty value")
  }

  case class Instruction(command: String, register: Char, value: Value)

  private val LineMatcher = "(snd|set|add|mul|mod|rcv|jgz) ([a-z])(?: (?:([a-z])|(-?\\d+)))?".r

  def instFromLine(line: String): Instruction = line match {
    case LineMatcher(c, r, ref, _) if ref != null =>
      Instruction(
        c,
        r.charAt(0),
        Ref(ref.charAt(0))
      )
    case LineMatcher(c, r, _, lit) if lit != null => Instruction(c, r.charAt(0), Literal(lit.toLong))
    case LineMatcher(c, r, _, _) => Instruction(c, r.charAt(0), EmptyValue)
  }

  def parseLines(lines: TraversableOnce[String]): Vector[Instruction] = lines.map(instFromLine).toVector

  def getRcv(program: Vector[Instruction]): Option[Long] = {

    def iter(registers: Map[Char, Long],
             position: Int,
             sent: Option[Long]): Option[Long] =

      if (!program.isDefinedAt(position)) None
      else program(position) match {
        case Instruction("snd", r, _) => iter(registers, position + 1, Some(registers(r)))
        case Instruction("set", r, v) => iter(registers.updated(r, v.getValue(registers)), position + 1, sent)
        case Instruction("add", r, v) => iter(registers.updated(r, registers(r) + v.getValue(registers)), position + 1, sent)
        case Instruction("mul", r, v) => iter(registers.updated(r, registers(r) * v.getValue(registers)), position + 1, sent)
        case Instruction("mod", r, v) => iter(registers.updated(r, registers(r) % v.getValue(registers)), position + 1, sent)
        case Instruction("jgz", r, v) if registers(r) > 0 => iter(registers, position + v.getValue(registers).toInt, sent)
        case Instruction("jgz", _, _) => iter(registers, position + 1, sent)
        case Instruction("rcv", r, _) if registers(r) != 0 => sent
        case Instruction("rcv", _, _) => iter(registers, position + 1, sent)
      }

    iter(Map.empty.withDefaultValue(0), 0, None)
  }


//  def program = Actor.immutable[Message] { (ctx, msg) =>
//
//  }

  def main(args: Array[String]): Unit = {
    val program = parseLines(Source.fromResource("day18input.txt").getLines().filter(l => l.matches(LineMatcher.regex)))

    println(getRcv(program))

    //val system: ActorSystem[Day18.Message] = ActorSystem()
  }
}

object Program {

  sealed trait Message

  case class Start(pair: ActorRef[Message]) extends Message

  case class Send(value: Long) extends Message

  case object Waiting extends Message

  case object Terminated extends Message

  def behavior(pid: Int, program: Vector[Instruction]): Behavior[Message] = {

    private case class State(registers: Map[Char, Long], position: Int, pairedWith: ActorRef[Message], sentCount: Int)

    def run(state: State): Behavior[Message] =
      if (!program.isDefinedAt(state.position)) {
        state.pairedWith ! Terminated
        println(s"Program $pid Terminated, sent count: ${state.sentCount}")
        Actor.stopped
      }
      else program(state.position) match {
        case Instruction("snd", r, _) =>
          state.pairedWith ! Send(state.registers(r))
          run(state.copy(position = state.position + 1, sentCount = state.sentCount + 1))
        case Instruction("set", r, v) =>
          run(state.copy(
            registers = state.registers.updated(r, v.getValue(state.registers)),
            position = state.position + 1
          ))
        case Instruction("add", r, v) =>
          run(state.copy(
            registers = state.registers.updated(r, state.registers(r) + v.getValue(state.registers)),
            position = state.position + 1
          ))
        case Instruction("mul", r, v) =>
          run(state.copy(
            registers = state.registers.updated(r, state.registers(r) * v.getValue(state.registers)),
            position = state.position + 1
          ))
        case Instruction("mod", r, v) =>
          run(state.copy(
            registers = state.registers.updated(r, state.registers(r) % v.getValue(state.registers)),
            position = state.position + 1
          ))
        case Instruction("jgz", r, v) if state.registers(r) > 0 =>
          run(state.copy(position = state.position + v.getValue(state.registers).toInt))
        case Instruction("jgz", _, _) =>
          run(state)
        case Instruction("rcv", r, _) =>
          awaitMsg(state, r)
      }

    def awaitMsg(state: State, target: Char, seenWaiting: Boolean = false): Behavior[Message] =
      Actor.immutable[Message] { (_, msg) =>
        msg match {
          case Send(value) => run(state.copy(registers = state.registers.updated(target, value)))
          case Waiting if seenWaiting =>
            state.pairedWith ! Terminated
            println(s"Program $pid Terminated, sent count: ${state.sentCount}")
            Actor.stopped
          case Waiting =>
            state.pairedWith ! Waiting
            awaitMsg(state, target, seenWaiting = true)
          case Terminated =>
            println(s"Program $pid Terminated, sent count: ${state.sentCount}")
            Actor.stopped
        }
      }

    Actor.immutable[Message] { (_, msg) =>
      msg match {
        case Start(pair) => run(State(Map('p' -> pid).withDefaultValue(0), 0, pair, 0))
        case _ => Actor.same
      }

    }
  }

}