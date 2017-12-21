import Day15._
import org.scalatest.{FunSuite, Matchers}

class Day15Test extends FunSuite with Matchers {

  test("can build generator") {
    buildIterator(65, 16807).take(5).toSeq shouldBe Seq(
      1092455,
      1181022009,
      245556042,
      1744312007,
      1352636452
    )

    buildIterator(8921, 48271).take(5).toSeq shouldBe Seq(
      430625591,
      1233683848,
      1431495498,
      137874439,
      285222916
    )
  }

  test("can compare bits") {
    buildLSBComparator(5)((1092455, 430625591)) shouldBe false
    buildLSBComparator(5)((1181022009, 1233683848)) shouldBe false
    buildLSBComparator(5)((245556042, 1431495498)) shouldBe true
    buildLSBComparator(5)((1744312007, 137874439)) shouldBe true
    buildLSBComparator(5)((1352636452, 285222916)) shouldBe true

    buildLSBComparator(16)((1092455, 430625591)) shouldBe false
    buildLSBComparator(16)((1181022009, 1233683848)) shouldBe false
    buildLSBComparator(16)((245556042, 1431495498)) shouldBe true
    buildLSBComparator(16)((1744312007, 137874439)) shouldBe false
    buildLSBComparator(16)((1352636452, 285222916)) shouldBe false
  }

  test("can count matches") {
    countMatches(
      buildIterator(65, 16807),
      buildIterator(8921, 48271),
      5
    ) shouldBe 1

    countMatches(
      buildIterator(65, 16807),
      buildIterator(8921, 48271),
      40000000
    ) shouldBe 588
  }

  test("can filter results") {
    filterIterator(65, 16807, 4).take(5).toSeq shouldBe Seq (
      1352636452,
      1992081072,
      530830436,
      1980017072,
      740335192
    )

    filterIterator(8921, 48271, 8).take(5).toSeq shouldBe Seq (
      1233683848,
      862516352,
      1159784568,
      1616057672,
      412269392
    )
  }
}
