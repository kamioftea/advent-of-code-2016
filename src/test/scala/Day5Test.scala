import org.scalatest.{FunSuite, Matchers}

class Day5Test extends FunSuite with Matchers {

  test("testCountMoves") {
    Day5.countMoves(Seq(0,3,0,1,-3)) shouldBe 5
    Day5.countMoves(Seq(1)) shouldBe 1
    Day5.countMoves(Seq(-1)) shouldBe 1
    Day5.countMoves(Seq(0)) shouldBe 2
  }

 test("testCountMovesVariant") {
    Day5.countMovesVariant(Seq(0,3,0,1,-3)) shouldBe 10
    Day5.countMovesVariant(Seq(1)) shouldBe 1
    Day5.countMovesVariant(Seq(-1)) shouldBe 1
    Day5.countMovesVariant(Seq(0)) shouldBe 2
    Day5.countMovesVariant(Seq(3, 1 , 2, -3)) shouldBe 4
  }

}
