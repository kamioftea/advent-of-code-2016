import Day22._
import org.scalatest.{FunSuite, Matchers}

class Day22Test extends FunSuite with Matchers {

  test("can parse input") {
    parseInput(
      """..#
        |#..
        |...""".stripMargin.lines.toSeq
    ) shouldBe Map(
      -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
      0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
      1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
    )
  }

  test("can count infections") {
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      7,
      basicStrategy
    ) shouldBe 5
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      70,
      basicStrategy
    ) shouldBe 41
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      10000,
      basicStrategy
    ) shouldBe 5587
  }

  test("can evolve"){
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      100,
      evolvedStrategy
    ) shouldBe 26
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      10000000,
      evolvedStrategy
    ) shouldBe 2511944
  }

}
