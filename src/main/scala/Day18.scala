import Day18.Instruction
import Program.Start
import akka.NotUsed
import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, ActorSystem, Behavior, Terminated}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

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
    case "jgz 1 3" => Instruction("jmp", 'z', Literal(3))
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

  def runInParallel(program: Vector[Instruction]): ActorSystem[NotUsed] = {
    val root: Behavior[NotUsed] =
      Actor.deferred { ctx =>
        val p0 = ctx.spawn(Program.behavior(0, program), "p0")
        val p1 = ctx.spawn(Program.behavior(1, program), "p1")

        ctx.watch(p0)
        ctx.watch(p1)

        p0 ! Start(p1)
        p1 ! Start(p0)

        def waiter(terminated: Set[ActorRef[_]]): Behavior[NotUsed] = Actor.immutable[NotUsed] {
          (_, _) => Actor.unhandled
        } onSignal {
          case (_, Terminated(ref)) =>
            println(ref + "Terminated")
            val newTerminated = terminated + ref
            if (newTerminated.size == 2) Actor.stopped
            else waiter(newTerminated)
        }

        waiter(Set.empty)
      }

    val system = ActorSystem(root, "Day18System")
    Await.result(system.whenTerminated, 30.seconds)

    system
  }

  case class Prog(label: String,
                  registers: Map[Char, Long],
                  position: Int,
                  sentCount: Int = 0,
                  term: Boolean = false,
                  target: Option[Char] = None)

  def runConcurrent(program: Vector[Instruction]): (Prog, Prog) = {

    def runUntilRecv(prog: Prog, incoming: Seq[Long], outgoing: Seq[Long]): (Prog, Seq[Long]) =
      if (prog.term || !program.isDefinedAt(prog.position)) (prog.copy(term = true), outgoing)
      else program(prog.position) match {
        case Instruction("snd", r, _) =>
          println(s"${prog.label} Send ${prog.registers(r)}")
          runUntilRecv(
            prog.copy(
              position = prog.position + 1,
              sentCount = prog.sentCount + 1
            ),
            incoming,
            outgoing :+ prog.registers(r)
          )
        case Instruction("set", r, v) =>
          runUntilRecv(
            prog.copy(
              registers = prog.registers.updated(r, v.getValue(prog.registers)),
              position = prog.position + 1
            ),
            incoming,
            outgoing
          )
        case Instruction("add", r, v) =>
          runUntilRecv(
            prog.copy(
              registers = prog.registers.updated(r, prog.registers(r) + v.getValue(prog.registers)),
              position = prog.position + 1
            ),
            incoming,
            outgoing
          )
        case Instruction("mul", r, v) =>
          runUntilRecv(
            prog.copy(
              registers = prog.registers.updated(r, prog.registers(r) * v.getValue(prog.registers)),
              position = prog.position + 1
            ),
            incoming,
            outgoing
          )
        case Instruction("mod", r, v) =>
          runUntilRecv(
            prog.copy(
              registers = prog.registers.updated(r, prog.registers(r) % v.getValue(prog.registers)),
              position = prog.position + 1
            ),
            incoming,
            outgoing
          )
        case Instruction("jgz", r, v) if prog.registers(r) > 0 =>
          runUntilRecv(
            prog.copy(
              position = prog.position + v.getValue(prog.registers).toInt
            ),
            incoming,
            outgoing
          )
        case Instruction("jgz", _, _) =>
          runUntilRecv(
            prog.copy(
              position = prog.position + 1
            ),
            incoming,
            outgoing
          )
        case Instruction("jmp", _, i) =>
          runUntilRecv(
            prog.copy(
              position = prog.position + i.getValue(prog.registers).toInt
            ),
            incoming,
            outgoing
          )
        case Instruction("rcv", r, _) =>
          println(s"${prog.label} rcv $incoming")
          incoming match {
            case i +: is => runUntilRecv(
              prog.copy(
                registers = prog.registers.updated(r, i),
                position = prog.position + 1
              ),
              is,
              outgoing
            )
            case Nil => (prog.copy(target = Some(r), position = prog.position + 1), outgoing)
          }
      }

    def iter(p0: Prog, p1: Prog, incoming: Seq[Long]): (Prog, Prog) = {
      println(incoming)

      if (incoming == Nil && p0.target.isDefined) (p0, p1)
      else {
        val (newP0, outgoing) = incoming match {
          case i +: is if p0.target.isDefined => runUntilRecv(
            p0.copy(
              registers = p0.registers.updated(p0.target.get, i),
              target = None
            ),
            is,
            Nil
          )
          case is => runUntilRecv(p0, is, Nil)
        }

        iter(p1, newP0, outgoing)
      }
    }

    iter(
      Prog("Prog 0", Map('p' -> 0l).withDefaultValue(0), 0),
      Prog("Prog 1", Map('p' -> 1l).withDefaultValue(0), 0),
      Nil
    )
  }

  def main(args: Array[String]): Unit = {

    val program = parseLines(Source.fromResource("day18input.txt").getLines())

    println(getRcv(program))
    println(runConcurrent(parseLines(
      """set x 1
        |set y 2
        |snd x
        |snd y
        |snd p
        |rcv a
        |rcv b
        |rcv c
        |rcv d""".stripMargin.lines)))

    println(runConcurrent(parseLines(
      """set y 10
        |mul y p
        |add y 10
        |snd y
        |rcv x
        |add y -1
        |jgz x -3
        |rcv b""".stripMargin.lines)))


   // println(runConcurrent(program))
    println(runInParallel(program))
  }
}

object Program {

  sealed trait Message

  case class Start(pair: ActorRef[Message]) extends Message

  case class Send(value: Long) extends Message

  case class Waiting(sentCount: Long, receivedCount: Long) extends Message

  case object Running extends Message

  case object Terminated extends Message

  private case class State(registers: Map[Char, Long],
                           position: Int,
                           pairedWith: ActorRef[Message],
                           sentCount: Int,
                           receivedCount: Int = 0)

  def behavior(pid: Int, program: Vector[Instruction]): Behavior[Message] = {

    def run(state: State): Behavior[Message] = {
      //      val state = xState.copy(count = xState.count + 1)
      //      if(state.count > 100) return Actor.stopped

      //      println(s"Program $pid : $state")

      if (!program.isDefinedAt(state.position)) {
        state.pairedWith ! Terminated
        println(s"Program $pid Terminated, sent count: ${state.sentCount} state: $state")
        Actor.stopped
      }
      else program(state.position) match {
        case Instruction("snd", r, _) =>
          state.pairedWith ! Send(state.registers(r))
          run(state.copy(
            position = state.position + 1,
            sentCount = state.sentCount + 1
          ))
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
          run(state.copy(position = state.position + 1))
        case Instruction("rcv", r, _) =>
          awaitMsg(state.copy(position = state.position + 1), r)
      }
    }

    def awaitMsg(state: State, target: Char): Behavior[Message] = {
      Actor.immutable[Message] { (_, msg) => {
        println(s"Program $pid : $msg ${state.receivedCount} ${state.sentCount} $target ${state.registers}")
        //state.pairedWith ! Waiting(state.sentCount, state.receivedCount)
        msg match {
          case Send(value) =>
            run(state.copy(
              registers = state.registers.updated(target, value),
              receivedCount = state.receivedCount + 1
            ))
          //          case Waiting(rsc, rrc) =>
          //            if(rsc == state.receivedCount && rrc == state.sentCount) {
          //              println(s"Program $pid Terminated due matching counts, ${state.sentCount}")
          //              Actor.stopped
          //            }
          //            else Actor.same
          case Terminated =>
            println(s"Program $pid Terminated due to other term, sent count: ${state.sentCount}")
            Actor.stopped
          case _ => Actor.same
        }
      }
      }
    }

    Actor.immutable[Message] { (_, msg) => {
      println(s"Program $pid : $msg")
      msg match {
        case Start(pair) => run(State(Map('p' -> pid.toLong).withDefaultValue(0), 0, pair, 0))
        case _ => Actor.same
      }
    }
    }
  }


}
