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

  def main(args: Array[String]): Unit = {
    val program = parseLines(Source.fromResource("day18input.txt").getLines().filter(l => l.matches(LineMatcher.regex)))

    println(getRcv(program))
  }
}
