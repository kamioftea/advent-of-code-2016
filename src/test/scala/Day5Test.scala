import Day5._
import org.scalatest.{FunSuite, Matchers}

class Day5Test extends FunSuite with Matchers {

  test("testCountMoves") {
    countMoves(Seq(0,3,0,1,-3)) shouldBe 5
    countMoves(Seq(1)) shouldBe 1
    countMoves(Seq(-1)) shouldBe 1
    countMoves(Seq(0)) shouldBe 2
  }

 test("testCountMovesVariant") {
    countMovesWithConvergence(Seq(0,3,0,1,-3)) shouldBe 10
    countMovesWithConvergence(Seq(1)) shouldBe 1
    countMovesWithConvergence(Seq(-1)) shouldBe 1
    countMovesWithConvergence(Seq(0)) shouldBe 2
    countMovesWithConvergence(Seq(3, 1 , 2, -3)) shouldBe 4
  }

}
