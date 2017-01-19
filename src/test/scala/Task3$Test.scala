import org.scalatest.{FunSuite, Matchers}

/**
  * Created by jeff on 19/01/2017.
  */
class Task3$Test extends FunSuite with Matchers {

  test("validates correctly") {
    Task3.validateTriangle(10, 10, 10) shouldBe true

    Task3.validateTriangle(2, 1, 1) shouldBe false
    Task3.validateTriangle(1, 2, 1) shouldBe false
    Task3.validateTriangle(1, 1, 2) shouldBe false

    Task3.validateTriangle(1, 2, 4) shouldBe false
    Task3.validateTriangle(1, 4, 2) shouldBe false
    Task3.validateTriangle(2, 1, 4) shouldBe false
    Task3.validateTriangle(2, 4, 1) shouldBe false
    Task3.validateTriangle(4, 1, 2) shouldBe false
    Task3.validateTriangle(4, 2, 1) shouldBe false

    Task3.validateTriangle(2, 3, 4) shouldBe true
    Task3.validateTriangle(2, 4, 3) shouldBe true
    Task3.validateTriangle(3, 2, 4) shouldBe true
    Task3.validateTriangle(3, 4, 2) shouldBe true
    Task3.validateTriangle(4, 2, 3) shouldBe true
    Task3.validateTriangle(4, 3, 2) shouldBe true

    Task3.validateTriangle(5, 10, 25) shouldBe false

    Task3.validateTriangle(883, 357, 185) shouldBe false
    Task3.validateTriangle(572, 189, 424) shouldBe true
    Task3.validateTriangle(842, 206, 272) shouldBe false
    Task3.validateTriangle(55, 656, 94) shouldBe false
    Task3.validateTriangle(612, 375, 90) shouldBe false
    Task3.validateTriangle(663, 550, 179) shouldBe true
    Task3.validateTriangle(183, 487, 470) shouldBe true
    Task3.validateTriangle(551, 664, 431) shouldBe true
  }

  test("counts correctly") {
    Task3.countPossibleTriangles(
      "1  1  1  \n" +
        "2  1  1  \n" +
        "1  2  1  \n" +
        "1  1  2  \n" +
        "1  2  4  \n" +
        "1  4  2  \n" +
        "  2  1  4  \n" +
        "2  4  1  \n" +
        "4  1  2  \n" +
        "4  2  1  \n" +
        "2  3  4  \n" +
        "2  4  3  \n" +
        " 3  2  4  \n" +
        "3  4  2  \n" +
        "4  2  3  \n" +
        "4  3  2  \n" +
        "5  10  25"
    ) shouldBe 7

    Task3.countPossibleTriangles(
      "  883  357  185\n" +
        "  572  189  424\n" +
        "  842  206  272\n" +
        "   55  656   94\n" +
        "  612  375   90\n" +
        "  663  550  179\n" +
        "  183  487  470\n" +
        "  551  664  431"
    ) shouldBe 4
  }

}
