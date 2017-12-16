import Day16._
import org.scalatest.{FunSuite, Matchers}

class Day16Test extends FunSuite with Matchers {

  test("testApplyInstruction") {
    applyInstruction("s1", "abcde".toVector) shouldBe "eabcd".toVector
    applyInstruction("x3/4", "eabcd".toVector) shouldBe "eabdc".toVector
    applyInstruction("pe/b", "eabdc".toVector) shouldBe "baedc".toVector
  }

  test("can apply instructions") {
    applyInstructions("s1,x3/4,pe/b".split(','), "abcde".toVector) shouldBe "baedc".toVector
  }
}
