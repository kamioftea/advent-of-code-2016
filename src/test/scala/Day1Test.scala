import Day1._
import org.scalatest.{FunSuite, Matchers}

class Day1Test extends FunSuite with Matchers {

    test("can Sum Duplicates") {
    sumDuplicates("1122".map(_.asDigit)) shouldBe 3
    sumDuplicates("1111".map(_.asDigit)) shouldBe 4
    sumDuplicates("1234".map(_.asDigit)) shouldBe 0
    sumDuplicates("91212129".map(_.asDigit)) shouldBe 9
    sumDuplicates("91222129".map(_.asDigit)) shouldBe 13
  }


  test("can Sum Mirrors") {
    sumMirrors("1212".map(_.asDigit)) shouldBe 6
    sumMirrors("1221".map(_.asDigit)) shouldBe 0
    sumMirrors("123425".map(_.asDigit)) shouldBe 4
    sumMirrors("123123".map(_.asDigit)) shouldBe 12
    sumMirrors("12131415".map(_.asDigit)) shouldBe 4
  }
}
