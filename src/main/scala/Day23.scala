import scala.io.Source
import scala.language.implicitConversions

object Day23 {

  sealed trait Value {
    def read(implicit registers: Map[Char, Long]): Long
  }

  case class Ref(register: Char) extends Value {
    override def read(implicit registers: Map[Char, Long]): Long =
      registers.getOrElse(register, 0)
  }

  case class Literal(value: Long) extends Value {
    override def read(implicit registers: Map[Char, Long]): Long = value
  }

  case class Instruction(command: String, value1: Value, value2: Value)

  implicit def valueFromString(str: String): Value =
    if (str.matches("[a-z]")) Ref(str.charAt(0))
    else Literal(str.toLong)

  private val LineMatcher = "(set|sub|mul|jnz) ([a-z]|-?\\d+) ([a-z]|-?\\d+)".r

  def instFromLine(line: String): Instruction = line match {
    case LineMatcher(c, v1, v2) => Instruction(c, v1, v2)
  }

  def countMults(program: Vector[Instruction], regs: Map[Char, Long] = Map.empty): Int = {
    def iter(registers: Map[Char, Long], position: Int, mulCount: Int): Int = {
      if (!program.isDefinedAt(position)) mulCount
      else {
        implicit val _ = registers
        program(position) match {
          case Instruction("set", Ref(r), v2) =>
            iter(registers.updated(r, v2.read), position + 1, mulCount)
          case Instruction("sub", Ref(r), v2) =>
            iter(registers.updated(r, registers(r) - v2.read), position + 1, mulCount)
          case Instruction("mul", Ref(r), v2) =>
            iter(registers.updated(r, registers(r) * v2.read), position + 1, mulCount + 1)
          case Instruction("jnz", v1, v2) =>
            iter(registers, if (v1.read != 0) position + v2.read.toInt else position + 1, mulCount)
        }
      }
    }

    iter(regs.withDefaultValue(0), 0, 0)
  }

  def decomposed(seed: Int): Int =
  {
    val start = seed * 100 + 100000
    val end = start + 17000

    def hasFactor(b: Int): Boolean = {
      val rootB = Math.sqrt(b).toInt

      def iter(d: Int): Boolean = {
        if(d > rootB) return false
        if(b % d == 0) return true
        iter(d + 1)
      }

      iter(2)
    }

    def iter(b: Int, h: Int = 0): Int = {
      if(b > end) return h
      iter(b + 17, if(hasFactor(b)) h + 1 else h)
    }

    iter(start)
  }

  def main(args: Array[String]): Unit = {
    val program =
      Source.fromResource("day23input.txt")
        .getLines()
        .map(instFromLine)
        .toVector

    println(countMults(program))

    println(decomposed(79))

  }
}
