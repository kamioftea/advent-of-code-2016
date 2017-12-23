import Day16._
import org.scalatest.{FunSuite, Matchers}

class Day16Test extends FunSuite with Matchers {

  test("can apply an instruction") {
    applyInstruction("s1", "abcde".toVector) shouldBe "eabcd".toVector
    applyInstruction("x3/4", "eabcd".toVector) shouldBe "eabdc".toVector
    applyInstruction("pe/b", "eabdc".toVector) shouldBe "baedc".toVector
  }

  test("can apply program") {
    applyInstructions("s1,x3/4,pe/b".split(','), "abcde".toVector) shouldBe "baedc".toVector
  }

  test("refactor count loop size") {
      countLoopSize(Vector(0, 2, 7, 0), Day6.redistribute)._2 shouldBe 4
      countLoopSize(Vector(2, 4, 1, 2), Day6.redistribute)._2 shouldBe 4
      countLoopSize(Vector(3, 1, 2, 3), Day6.redistribute)._2 shouldBe 4
      countLoopSize(Vector(23), Day6.redistribute)._2 shouldBe 1
      countLoopSize(Vector(4, 2), Day6.redistribute)._2 shouldBe 2
      countLoopSize(Vector(5, 1), Day6.redistribute)._2 shouldBe 2
  }
}
