import scala.io.Source

object Day25 {

  case class Action(write: Boolean, move: Boolean, next: Char)

  case class State(on0: Action, on1: Action)

  case class Program(state: Char, iterations: Int, states: Map[Char, State]) {
    lazy val startingState: State = states(state)

  }

  def parseInput(lines: Seq[String]): Program = {
    val StateLine = "Begin in state ([A-Z]).".r
    val state: Char = lines.head match {
      case StateLine(s) => s.charAt(0)
    }

    val IterationsLine = "Perform a diagnostic checksum after (\\d+) steps.".r
    val iterations: Int = lines.drop(1).head match {
      case IterationsLine(i) => i.toInt
    }

    val StateHeader = "In state ([A-Z]):".r
    val ActionHeader = "  If the current value is (0|1):".r
    val WriteLine = "    - Write the value (0|1).".r
    val MoveLine = "    - Move one slot to the (right|left).".r
    val NextStateLine = "    - Continue with state ([A-Z]).".r

    def parseState(s: Seq[String]): (Char, State) = s match {
      case
        StateHeader(l) +:
          ActionHeader("0") +:
          WriteLine(w0) +:
          MoveLine(m0) +:
          NextStateLine(ns0) +:
          ActionHeader("1") +:
          WriteLine(w1) +:
          MoveLine(m1) +:
          NextStateLine(ns1) +: _ =>
        (l.charAt(0), State(
          Action(w0 == "1", m0 == "right", ns0.charAt(0)),
          Action(w1 == "1", m1 == "right", ns1.charAt(0))
        ))
    }

    def parseStates(states: Seq[Seq[String]],
                    map: Map[Char, State] = Map.empty): Map[Char, State] = {
      states match {
        case Nil => map
        case s +: ss => parseStates(ss, map + parseState(s))
      }
    }

    val states = parseStates(lines.drop(3).grouped(10).toSeq)

    Program(state, iterations, states)
  }

  def debugChecksum(program: Program): Int = {
    def iter(pos: Int, state: State, ribbon: Set[Int], iterations: Int): Int =
      if (iterations >= program.iterations)
        ribbon.size
      else {
        val action: Action = if (ribbon.contains(pos)) state.on1 else state.on0
        iter(
          pos + (if (action.move) 1 else -1),
          program.states(action.next),
          if (action.write) ribbon + pos else ribbon - pos,
          iterations + 1
        )
      }


    iter(0, program.startingState, Set.empty, 0)
  }

  def main(args: Array[String]): Unit = {
    val program = parseInput(Source.fromResource("day25input.txt").getLines().toSeq)

    println(debugChecksum(program))
  }
}
