import Day14._
import org.scalatest.{FunSuite, Matchers}

class Day14Test extends FunSuite with Matchers {

  test("testCountBits") {
    countBits("0") shouldBe 0
    countBits("1") shouldBe 1
    countBits("e") shouldBe 3
    countBits("f") shouldBe 4
    countBits("ff") shouldBe 8
    countBits("1248") shouldBe 4
    countBits("137f") shouldBe 10
    countBits("f0000000000000000000000000000001") shouldBe 5
  }

  test("testCountUsed") {
    countUsed("flqrgnkx") shouldBe 8108
  }

  test("testCountRegions") {
    countRegions("flqrgnkx") shouldBe 1242
  }

}
