import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  sealed trait Instruction

  case class Push(state: State) extends Instruction

  case object Pop

  sealed trait State {
    def apply(char: Char): State
  }

  case object ExpectOpen extends State {
    override def apply(char: Char): State = char match {
      case '{' => StartGroup
      case '<' => StartGarbage
      case _ => Error("ExpectOpen: expected '{' or '<'; got " + char)
    }
  }

  case object StartGroup extends State {
    override def apply(char: Char): State = char match {
      case '{' => StartGroup
      case '<' => StartGarbage
      case '}' => CloseGroup
      case _ => Error("ExpectOpen: expected '{', '}', or '<'; got " + char)
    }
  }

  case object CloseGroup extends State {
    override def apply(char: Char): State = char match {
      case ',' => ExpectOpen
      case '}' => CloseGroup
      case _ => Error("StartGroup: expected '}' or ','; got " + char)
    }
  }

  case object StartGarbage extends State {
    override def apply(char: Char): State = char match {
      case '>' => CloseGarbage
      case '!' => Escaped
      case _ => InGarbage
    }
  }

  case object InGarbage extends State {
    override def apply(char: Char): State = char match {
      case '>' => CloseGarbage
      case '!' => Escaped
      case _ => InGarbage
    }
  }

  case object Escaped extends State {
    override def apply(char: Char): State = StartGarbage
  }

  case object CloseGarbage extends State {
    override def apply(char: Char): State = char match {
      case ',' => ExpectOpen
      case '}' => CloseGroup
      case _ => Error("StartGroup: expected '}' or ','; got " + char)
    }
  }

  case class Error(msg: String) extends State {
    override def apply(char: Char): State = this
  }

  def parseStream(chars: Stream[Char]): Stream[State] = {

    lazy val states: Stream[State] =
      ExpectOpen #:: states.zip(chars).map { case (s, c) => s(c) }

    states
  }

  def scoreGroups(states: Stream[State]): Int = {
    @tailrec
    def iter(states: Stream[State], depth: Int = 0, score: Int = 0): Int =
      states match {
        case Stream.Empty => score
        case StartGroup #:: tail => iter(tail, depth + 1, score)
        case CloseGroup #:: tail => iter(tail, depth - 1, score + depth)
        case _ #:: tail => iter(tail, depth, score)
      }

    iter(states)
  }

  def countGarbageChars(states: Stream[State]): Int =
    states.count(_ == InGarbage)

  def main(args: Array[String]): Unit = {
    def input = Source.fromResource("day9input.txt").toStream

    println(scoreGroups(parseStream(input)))
    println(countGarbageChars(parseStream(input)))
  }

}
