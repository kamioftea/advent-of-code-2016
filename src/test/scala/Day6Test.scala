import org.scalatest.{FunSuite, Matchers}

class Day6Test extends FunSuite with Matchers {

  test("testCountUntilLoop") {
    Day6.countUntilLoop(Vector(0, 2, 7, 0)) shouldBe 5
    Day6.countUntilLoop(Vector(2, 4, 1, 2)) shouldBe 4
    Day6.countUntilLoop(Vector(3, 1, 2, 3)) shouldBe 4
    Day6.countUntilLoop(Vector(23)) shouldBe 1
    Day6.countUntilLoop(Vector(4, 2)) shouldBe 2
    Day6.countUntilLoop(Vector(5, 1)) shouldBe 3
  }
  test("testCountLoopSize") {
    Day6.countLoopSize(Vector(0, 2, 7, 0)) shouldBe 4
    Day6.countLoopSize(Vector(2, 4, 1, 2)) shouldBe 4
    Day6.countLoopSize(Vector(3, 1, 2, 3)) shouldBe 4
    Day6.countLoopSize(Vector(23)) shouldBe 1
    Day6.countLoopSize(Vector(4, 2)) shouldBe 2
    Day6.countLoopSize(Vector(5, 1)) shouldBe 2
  }

  test("testRedistribute") {
    Day6.redistribute(Vector(0, 2, 7, 0)) shouldBe Vector(2, 4, 1, 2)
    Day6.redistribute(Vector(2, 4, 1, 2)) shouldBe Vector(3, 1, 2, 3)
    Day6.redistribute(Vector(23)) shouldBe Vector(23)
    Day6.redistribute(Vector(4, 2)) shouldBe Vector(2, 4)
  }
}
