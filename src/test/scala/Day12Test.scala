import Day12._
import org.scalatest.{FunSuite, Matchers}

class Day12Test extends FunSuite with Matchers {
  test("Can parse input") {
    parseInput(
      """0 <-> 2
        |1 <-> 1
        |2 <-> 0, 3, 4
        |3 <-> 2, 4
        |4 <-> 2, 3, 6
        |5 <-> 6
        |6 <-> 4, 5""".stripMargin.lines
    ) shouldBe Vector(
      Set(2),
      Set(1),
      Set(0, 3, 4),
      Set(2, 4),
      Set(2, 3, 6),
      Set(6),
      Set(4, 5)
    )
  }

  test("Can find size of cluster") {
    clusterWith(
      0,
      Vector(
        Set(2),
        Set(1),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3, 6),
        Set(6),
        Set(4, 5)
      )
    ).size shouldBe 6

    clusterWith(
      0,
      Vector(
        Set(2),
        Set(1),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3),
        Set(6),
        Set(5)
      )
    ).size shouldBe 4

    clusterWith(
      0,
      Vector(
        Set(2),
        Set(6),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3),
        Set(6),
        Set(1, 5)
      )
    ).size shouldBe 4

    clusterWith(
      0,
      Vector(
        Set(2),
        Set(6),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3),
        Set(6),
        Set(1, 5)
      )
    ).size shouldBe 4

    clusterWith(
      1,
      Vector(
        Set(2),
        Set(6),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3),
        Set(6),
        Set(1, 5)
      )
    ).size shouldBe 3
  }

  test("Can count clusters") {
    countClusters(
      Vector(
        Set(2),
        Set(1),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3, 6),
        Set(6),
        Set(4, 5)
      )
    ) shouldBe 2

    countClusters(
      Vector(
        Set(0)
      )
    ) shouldBe 1

    countClusters(
      Vector(
        Set(1),
        Set(0)
      )
    ) shouldBe 1

    countClusters(
      Vector(
        Set(0),
        Set(1)
      )
    ) shouldBe 2

    countClusters(
      Vector(
        Set(1, 2),
        Set(0),
        Set(0)
      )
    ) shouldBe 1

    countClusters(
      Vector(
        Set(0),
        Set(1),
        Set(2)
      )
    ) shouldBe 3

    countClusters(
      Vector(
        Set(2),
        Set(6),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3),
        Set(6),
        Set(1, 5)
      )
    ) shouldBe 2

    countClusters(
      Vector(
        Set(2),
        Set(1),
        Set(0, 3, 4),
        Set(2, 4),
        Set(2, 3),
        Set(6),
        Set(5)
      )
    ) shouldBe 3

    countClusters(
      Vector(
        Set(2),
        Set(3),
        Set(0, 4),
        Set(1),
        Set(2),
        Set(6),
        Set(5)
      )
    ) shouldBe 3

  }
}
