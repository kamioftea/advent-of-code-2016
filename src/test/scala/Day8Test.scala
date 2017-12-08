import Day8._
import org.scalatest.{FunSuite, Matchers}

import scala.util.Success

class Day8Test extends FunSuite with Matchers {

  test("canParseInstruction") {
    Instruction.fromLine("b inc 5 if a > 1") shouldBe Success(Instruction("b", Inc, 5, "a", Gt, 1))
    Instruction.fromLine("a inc 1 if b < 5") shouldBe Success(Instruction("a", Inc, 1, "b", Lt, 5))
    Instruction.fromLine("c dec -10 if a >= 1") shouldBe Success(Instruction("c", Dec, -10, "a", Gte, 1))
    Instruction.fromLine("c inc -20 if c == 10") shouldBe Success(Instruction("c", Inc, -20, "c", Eq, 10))
  }

  test("canApplyInstruction") {
    val emptyRegister = Map.empty[RegisterRef, Int].withDefaultValue(0)

    Instruction.fromLine("b inc 5 if a > 1").map(_ (emptyRegister)) shouldBe Success(emptyRegister)
    Instruction.fromLine("b inc 5 if a > 1").map(_ (Map("a" -> 1).withDefaultValue(0))) shouldBe Success(Map("a" -> 1))
    Instruction.fromLine("b inc 5 if a > 1").map(_ (Map("a" -> 2).withDefaultValue(0))) shouldBe Success(Map("a" -> 2, "b" -> 5))

    Instruction.fromLine("a inc 1 if b < 5").map(_ (emptyRegister)) shouldBe Success(Map("a" -> 1))
    Instruction.fromLine("a inc 1 if b < 5").map(_ (Map("b" -> 4).withDefaultValue(0))) shouldBe Success(Map("a" -> 1, "b" -> 4))
    Instruction.fromLine("a inc 1 if b < 5").map(_ (Map("b" -> 5).withDefaultValue(0))) shouldBe Success(Map("b" -> 5))

    Instruction.fromLine("c dec -10 if a >= 1").map(_ (emptyRegister)) shouldBe Success(emptyRegister)
    Instruction.fromLine("c dec -10 if a >= 1").map(_ (Map("a" -> 1).withDefaultValue(0))) shouldBe Success(Map("a" -> 1, "c" -> 10))
    Instruction.fromLine("c dec -10 if a >= 1").map(_ (Map("a" -> 2).withDefaultValue(0))) shouldBe Success(Map("a" -> 2, "c" -> 10))

    Instruction.fromLine("c dec -10 if a <= 1").map(_ (emptyRegister)) shouldBe Success(Map("c" -> 10))
    Instruction.fromLine("c dec -10 if a <= 1").map(_ (Map("a" -> 1).withDefaultValue(0))) shouldBe Success(Map("a" -> 1, "c" -> 10))
    Instruction.fromLine("c dec -10 if a <= 1").map(_ (Map("a" -> 2).withDefaultValue(0))) shouldBe Success(Map("a" -> 2))

    Instruction.fromLine("c inc -20 if c == 10").map(_ (emptyRegister)) shouldBe Success(emptyRegister)
    Instruction.fromLine("c inc -20 if c == 10").map(_ (Map("c" -> 10).withDefaultValue(0))) shouldBe Success(Map("c" -> -10))
    Instruction.fromLine("c inc -20 if c == 10").map(_ (Map("c" -> 11, "d" -> 23).withDefaultValue(0))) shouldBe Success(Map("c" -> 11, "d" -> 23))

    Instruction.fromLine("a dec 20 if c != 10").map(_ (emptyRegister)) shouldBe Success(Map("a" -> -20))
    Instruction.fromLine("a dec 20 if c != 10").map(_ (Map("c" -> 10, "d" -> 23).withDefaultValue(0))) shouldBe Success(Map("c" -> 10, "d" -> 23))
    Instruction.fromLine("a dec 20 if c != 10").map(_ (Map("c" -> 11, "a" -> 23).withDefaultValue(0))) shouldBe Success(Map("c" -> 11, "a" -> 3))
  }

  test("canApplyInstructions") {
    val instructions: Iterator[Instruction] =
      """b inc 5 if a > 1
        |a inc 1 if b < 5
        |c dec -10 if a >= 1
        |c inc -20 if c == 10""".stripMargin.lines
        .map(Instruction.fromLine)
        .collect { case Success(i) => i }

    applyInstructions(Map.empty[RegisterRef, Int].withDefaultValue(0), instructions) shouldBe Map("a"->1, "c" -> -10)
  }

  test("canFindMax") {
    findMax(Map("a"->1, "c" -> -10)) should be ("a", 1)
  }

  test("canFindProcessingMax") {
    val instructions: Iterator[Instruction] =
      """b inc 5 if a > 1
        |a inc 1 if b < 5
        |c dec -10 if a >= 1
        |c inc -20 if c == 10""".stripMargin.lines
        .map(Instruction.fromLine)
        .collect { case Success(i) => i }

    findProcessingMax(Map.empty[RegisterRef, Int].withDefaultValue(0), instructions) shouldBe 10
  }

}
