import Day17._
import org.scalatest.{FunSuite, Matchers}

class Day17Test extends FunSuite with Matchers {

  test("can spin buffer") {
    spin(0, 3) shouldBe (0, Vector(0))
    spin(1, 3) shouldBe (1, Vector(0,1))
    spin(2, 3) shouldBe (1, Vector(0,2,1))
    spin(3, 3) shouldBe (2, Vector(0,2,3,1))
    spin(4, 3) shouldBe (2, Vector(0,2,4,3,1))
    spin(5, 3) shouldBe (1, Vector(0,5,2,4,3,1))
    spin(6, 3) shouldBe (5, Vector(0,5,2,4,3,6,1))
    spin(7, 3) shouldBe (2, Vector(0,5,7,2,4,3,6,1))
    spin(8, 3) shouldBe (6, Vector(0,5,7,2,4,3,8,6,1))
    spin(9, 3) shouldBe (1, Vector(0,9,5,7,2,4,3,8,6,1))
  }
  
  test("can determine following value") {
    getSubsequentValue(0, 3) shouldBe 0
    getSubsequentValue(1, 3) shouldBe 0
    getSubsequentValue(2, 3) shouldBe 1
    getSubsequentValue(3, 3) shouldBe 1
    getSubsequentValue(4, 3) shouldBe 3
    getSubsequentValue(5, 3) shouldBe 2
    getSubsequentValue(6, 3) shouldBe 1
    getSubsequentValue(7, 3) shouldBe 2
    getSubsequentValue(8, 3) shouldBe 6
    getSubsequentValue(9, 3) shouldBe 5

    getSubsequentValue(2017, 3) shouldBe 638
  }
  
  test("can track number after zero")
  {
    trackZero(0, 3) shouldBe 0
    trackZero(1, 3) shouldBe 1
    trackZero(2, 3) shouldBe 2
    trackZero(3, 3) shouldBe 2
    trackZero(4, 3) shouldBe 2
    trackZero(5, 3) shouldBe 5
    trackZero(6, 3) shouldBe 5
    trackZero(7, 3) shouldBe 5
    trackZero(8, 3) shouldBe 5
    trackZero(9, 3) shouldBe 9

    val (_, buffer) = spin(2017, 3)

    trackZero(2017, 3) shouldBe buffer(buffer.indexOf(0) + 1)
  }

}
