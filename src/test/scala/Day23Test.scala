import Day23._
import org.scalatest.{FunSuite, Matchers}

class Day23Test extends FunSuite with Matchers {

  test("testInstFromLine") {
    instFromLine("sub a 23") shouldBe Instruction("sub", Ref('a'), Literal(23))
    instFromLine("mul b -2") shouldBe Instruction("mul", Ref('b'), Literal(-2))
    instFromLine("set b 4") shouldBe Instruction("set", Ref('b'), Literal(4))
    instFromLine("jnz c d") shouldBe Instruction("jnz", Ref('c'), Ref('d'))
    instFromLine("jnz 1 2") shouldBe Instruction("jnz", Literal(1), Literal(2))
  }

  test("testCountMults") {
    countMults(
      """set a 4
        |set b 1
        |mul b a
        |jnz a 2
        |jnz 1 3
        |sub a 1
        |jnz 1 -4""".stripMargin.lines.map(instFromLine).toVector
    ) shouldBe 5

    countMults(
      """set a 8
        |set b 1
        |mul b a
        |jnz a 2
        |jnz 1 3
        |sub a 1
        |jnz 1 -4""".stripMargin.lines.map(instFromLine).toVector
    ) shouldBe 9
  }

}
