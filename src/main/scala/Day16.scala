import scala.annotation.tailrec
import scala.io.Source

object Day16 {

  private val Spin = "s(\\d+)".r
  private val Exchange = "x(\\d+)/(\\d+)".r
  private val Partner = "p([a-z])/([a-z])".r

  def applyInstruction(instruction: String, programs: Vector[Char]): Vector[Char] =
    instruction match {
      case Spin(n) => programs.takeRight(n.toInt) ++ programs.dropRight(n.toInt)
      case Exchange(a,b) =>
        programs
          .updated(a.toInt, programs(b.toInt))
          .updated(b.toInt, programs(a.toInt))
      case Partner(a, b) =>
        programs
            .updated(programs.indexOf(a.charAt(0)), b.charAt(0))
            .updated(programs.indexOf(b.charAt(0)), a.charAt(0))

    }
  def applyInstructions(instructions: Array[String], programs: Vector[Char]): Vector[Char] =
    instructions.foldLeft(programs){ case( ps, i) => applyInstruction(i, ps) }

  def countLoopSize[T](sequence: T, update: T => T): (Int, Int, Map[T, Int]) = {
    @tailrec
    def iter(state: T,
             seen: Map[T, Int] = Map.empty,
             count: Int = 0): (Int, Int, Map[T, Int]) =
      if (seen.isDefinedAt(state)) (count, count - seen(state), seen)
      else iter(update(state), seen.updated(state, count), count + 1)

    iter(sequence)
  }

  def applyRepeatInstructions(n: Int, instructions: Array[String], programs: Vector[Char]): Vector[Char] = {
    val (count, loopSize, seen) = countLoopSize(programs, (ps:Vector[Char]) => applyInstructions(instructions, ps))
    val prefix = count - loopSize

    seen.find { case (_, i) => i == prefix + ((n - prefix) % loopSize)}.get._1
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day16input.txt").mkString.trim.split(',')

    //noinspection SpellCheckingInspection
    println(applyInstructions(input, "abcdefghijklmnop".toVector).mkString)
    //noinspection SpellCheckingInspection
    println(applyRepeatInstructions(1000000000, input, "abcdefghijklmnop".toVector).mkString)


  }

}
