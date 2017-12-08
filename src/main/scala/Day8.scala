import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day8 {
  type RegisterRef = String

  sealed trait Operator {
    def apply(a: Int, b: Int): Int
  }

  case object Inc extends Operator {
    override def apply(a: Int, b: Int): Int = a + b
  }

  case object Dec extends Operator {
    def apply(a: Int, b: Int): Int = a - b
  }

  sealed trait Condition {
    def test(a: Int, b: Int): Boolean
  }

  object Condition {
    def fromString(str: String): Condition = str match {
      case "==" => Eq
      case "!=" => Neq
      case "<" => Lt
      case "<=" => Lte
      case ">" => Gt
      case ">=" => Gte
    }
  }

  case object Eq extends Condition {
    override def test(a: Int, b: Int): Boolean = a == b
  }

  case object Neq extends Condition {
    override def test(a: Int, b: Int): Boolean = a != b
  }

  case object Lt extends Condition {
    override def test(a: Int, b: Int): Boolean = a < b
  }

  case object Lte extends Condition {
    override def test(a: Int, b: Int): Boolean = a <= b
  }

  case object Gt extends Condition {
    override def test(a: Int, b: Int): Boolean = a > b
  }

  case object Gte extends Condition {
    override def test(a: Int, b: Int): Boolean = a >= b
  }

  object Instruction {
    private val LineParser = "([a-z]+) (inc|dec) (-?\\d+) if ([a-z]+) ([!<>=]{1,2}) (-?\\d+)".r

    def fromLine(line: String): Try[Instruction] = {
      line match {
        case LineParser(register, operator, value, conditionRegister, condition, conditionValue) =>
          Success(Instruction(
            register,
            if (operator == "inc") Inc else Dec,
            value.toInt,
            conditionRegister,
            Condition.fromString(condition),
            conditionValue.toInt
          ))

        case _ => Failure(new RuntimeException("line was not a valid instruction"))
      }
    }
  }

  case class Instruction(register: RegisterRef,
                         operator: Operator,
                         value: Int,
                         conditionRegister: RegisterRef,
                         condition: Condition,
                         conditionValue: Int) {
    def apply(registers: Map[RegisterRef, Int]): Map[RegisterRef, Int] =
      if (condition.test(registers(conditionRegister), conditionValue))
        registers.updated(register, operator(registers(register), value))
      else
        registers
  }

  def applyInstructions(registers: Map[RegisterRef, Int], instructions: TraversableOnce[Instruction]): Map[RegisterRef, Int] =
    instructions.foldLeft(registers){ case (rs, ins) => ins(rs) }

  def findMax(registers: Map[RegisterRef, Int]): (RegisterRef, Int) =
    if(registers.isEmpty) ("", Int.MinValue) else registers.maxBy{ case (_, v) => v }

  def findProcessingMax(registers: Map[RegisterRef, Int], instructions: TraversableOnce[Instruction]): Int =
    instructions.foldLeft((Int.MinValue, registers)){
      case ((max, rs), ins) =>
        val newRs =  ins(rs)
        (Math.max(max, findMax(newRs)._2), newRs)
    }._1

  def main(args: Array[String]): Unit = {
    val registers = Map.empty[RegisterRef, Int].withDefaultValue(0)
    def instructions: Iterator[Instruction] =
      Source.fromResource("day8input.txt")
        .getLines()
        .map(Instruction.fromLine)
        .collect { case Success(i) => i }

    println(findMax(applyInstructions(registers, instructions)))
    println(findProcessingMax(registers, instructions))
  }

}
