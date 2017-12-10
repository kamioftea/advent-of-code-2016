import org.scalatest.{FunSuite, Matchers}

class Day10Test extends FunSuite with Matchers {

  test("testHash") {
    Day10.hash(List(), 5) shouldBe Vector(0, 1, 2, 3, 4)
    Day10.hash(List(3), 5) shouldBe Vector(2, 1, 0, 3, 4)
    Day10.hash(List(3, 4), 5) shouldBe Vector(4, 3, 0, 1, 2)
    Day10.hash(List(3, 4, 1), 5) shouldBe Vector(4, 3, 0, 1, 2)
    Day10.hash(List(3, 4, 1, 5), 5) shouldBe Vector(3, 4, 2, 1, 0)
  }

  test("testKnotHash")
  {
    Day10.knotHash("") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    Day10.knotHash("AoC 2017") shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    Day10.knotHash("1,2,3") shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    Day10.knotHash("1,2,4") shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
  }
}
