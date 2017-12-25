import Day24._
import org.scalatest.{FunSuite, Matchers}

class Day24Test extends FunSuite with Matchers{

  test("can parse input") {
    parseInput(
      """0/2
        |2/2
        |2/3
        |3/4
        |3/5
        |0/1
        |10/1
        |9/10""".stripMargin.lines.toSeq
    ) shouldBe Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    )
  }

  test("can build bridges") {
    val bridges = buildBridges(Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    ))

    bridges should contain(Seq(Component(5,0,1)))
    bridges should contain(Seq(Component(6, 10, 1), Component(5,0,1)))
    bridges should contain(Seq(Component(7, 9, 10), Component(6, 10, 1), Component(5, 0, 1)))
    bridges should contain(Seq(Component(0, 0, 2)))
    bridges should contain(Seq(Component(2, 2, 3), Component(0, 0, 2)))

    bridges should contain(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse)
  }

  test("can score bridges") {
    scoreBridge(Seq(Component(5,0,1))) shouldBe 1
    scoreBridge(Seq(Component(6, 10, 1), Component(5,0,1))) shouldBe 12
    scoreBridge(Seq(Component(7, 9, 10), Component(6, 10, 1), Component(5, 0, 1))) shouldBe 31
    scoreBridge(Seq(Component(0, 0, 2))) shouldBe 2
    scoreBridge(Seq(Component(2, 2, 3), Component(0, 0, 2))) shouldBe 7
    scoreBridge(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse) shouldBe 14
    scoreBridge(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse) shouldBe 15
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2)).reverse) shouldBe 6
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3)).reverse) shouldBe 11
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse) shouldBe 18
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse) shouldBe 19
  }

  test("can find strongest bridge") {
    strongestBridge(Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    )) shouldBe 31
  }

  test("can find longest bridge") {
    longestBridge(Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    )) shouldBe 19
  }
}
