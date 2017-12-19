import Day19._
import org.scalatest.{FunSuite, Matchers}

class Day19Test extends FunSuite with Matchers {

  private val testGrid =
    """|     |
       |     |  +--+
       |     A  |  C
       | F---|----E|--+
       |     |  |  |  D
       |     +B-+  +--+""".stripMargin.lines.toVector

  test("testFindStart") {
    findStart(testGrid) shouldBe 5
  }

  test("testFindPath") {
    findPath(testGrid) shouldBe ("ABCDEF", 38)
  }

}
