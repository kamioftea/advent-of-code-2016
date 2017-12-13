import Day13._
import org.scalatest.{FunSuite, Matchers}

class Day13Test extends FunSuite with Matchers {

  test("testParseLines") {
    parseLines(
      """0: 3
        |1: 2
        |4: 4
        |6: 4
      """.stripMargin.lines
    ) shouldBe Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 4),
      Layer(6, 4)
    )
  }

  test("testCalcTripSeverity") {
    calcTripSeverity(Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 4),
      Layer(6, 4)
    )) shouldBe 24

    calcTripSeverity(Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 3),
      Layer(5, 7),
      Layer(6, 4)
    )) shouldBe 36
  }

  test("testCalcSafeTrip") {
    calcSafeTrip(Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 4),
      Layer(6, 4)
    )) shouldBe 10
  }

}
