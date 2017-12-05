import Day1._
import org.scalatest.{FunSuite, Matchers}

class Day1Test extends FunSuite with Matchers {

  test("can Sum Duplicates") {
    sumDuplicates("1122") shouldBe 3
    sumDuplicates("1111") shouldBe 4
    sumDuplicates("1234") shouldBe 0
    sumDuplicates("91212129") shouldBe 9
    sumDuplicates("91222129") shouldBe 13
  }


  test("can Sum Mirrors") {
    sumMirrors("1212") shouldBe 6
    sumMirrors("1221") shouldBe 0
    sumMirrors("123425") shouldBe 4
    sumMirrors("123123") shouldBe 12
    sumMirrors("12131415") shouldBe 4
  }
}
