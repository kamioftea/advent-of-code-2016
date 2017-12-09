import Day9._
import org.scalatest.{FunSuite, Matchers}

class Day9Test extends FunSuite with Matchers {

  test("canParseStream") {

    def repeat[T](n: Int, value: T): Stream[T] = {
      if (n == 0) Stream.empty
      else value #:: repeat(n - 1, value)
    }

    Day9.parseStream("<>,".toStream) shouldBe Stream(ExpectOpen, StartGarbage, CloseGarbage, ExpectOpen)

    Day9.parseStream("<random characters>,".toStream) shouldBe Stream(
      Stream(ExpectOpen, StartGarbage),
      repeat(17, InGarbage),
      Stream(CloseGarbage, ExpectOpen)
    ).flatten

    Day9.parseStream("<<<<>,".toStream) shouldBe Stream(
      Stream(ExpectOpen, StartGarbage),
      repeat(3, InGarbage),
      Stream(CloseGarbage, ExpectOpen)
    ).flatten

    Day9.parseStream("<{!>}>,".toStream) shouldBe Stream(
      ExpectOpen,
      StartGarbage,
      InGarbage,
      Escaped,
      StartGarbage,
      InGarbage,
      CloseGarbage,
      ExpectOpen
    )

    Day9.parseStream("<!!>,".toStream) shouldBe Stream(
      ExpectOpen,
      StartGarbage,
      Escaped,
      StartGarbage,
      CloseGarbage,
      ExpectOpen
    )

    Day9.parseStream("<!!!>>,".toStream) shouldBe Stream(
      ExpectOpen,
      StartGarbage,
      Escaped,
      StartGarbage,
      Escaped,
      StartGarbage,
      CloseGarbage,
      ExpectOpen
    )

    Day9.parseStream("<{o\"i!a,<{i<a>,".toStream) shouldBe Stream(
      Stream(ExpectOpen, StartGarbage),
      repeat(4, InGarbage),
      Stream(Escaped, StartGarbage),
      repeat(6, InGarbage),
      Stream(CloseGarbage, ExpectOpen)
    ).flatten

    Day9.parseStream("{},".toStream) shouldBe Stream(
      ExpectOpen,
      StartGroup,
      CloseGroup,
      ExpectOpen
    )

    Day9.parseStream("{},".toStream).count(_ == StartGroup) shouldBe 1

    Day9.parseStream("{{{}}},".toStream) shouldBe Stream(
      Stream(ExpectOpen),
      repeat(3, StartGroup),
      repeat(3, CloseGroup),
      Stream(ExpectOpen)
    ).flatten

    Day9.parseStream("{{{}}},".toStream).count(_ == StartGroup) shouldBe 3

    Day9.parseStream("{{},{}},".toStream) shouldBe Stream(
      Stream(ExpectOpen),
      repeat(2, StartGroup),
      Stream(CloseGroup, ExpectOpen, StartGroup),
      repeat(2, CloseGroup),
      Stream(ExpectOpen)
    ).flatten

    Day9.parseStream("{{{}}},".toStream).count(_ == StartGroup) shouldBe 3

    Day9.parseStream("{{{},{},{{}}}},".toStream) shouldBe Stream(
      Stream(ExpectOpen),
      repeat(2, StartGroup),
      repeat(2, Stream(StartGroup, CloseGroup, ExpectOpen)).flatten,
      repeat(2, StartGroup),
      repeat(4, CloseGroup),
      Stream(ExpectOpen)
    ).flatten

    Day9.parseStream("{{{},{},{{}}}},".toStream).count(_ == StartGroup) shouldBe 6

    Day9.parseStream("{<a>,<a>,<a>,<a>},".toStream).count(_ == StartGroup) shouldBe 1
    Day9.parseStream("{{<a>},{<a>},{<a>},{<a>}},".toStream).count(_ == StartGroup) shouldBe 5
    Day9.parseStream("{{<!>},{<!>},{<!>},{<a>}},".toStream).count(_ == StartGroup) shouldBe 2

  }

  test("canScoreGroups") {

    Day9.scoreGroups(Day9.parseStream("{},".toStream)) shouldBe 1
    Day9.scoreGroups(Day9.parseStream("{{{}}},".toStream)) shouldBe 6
    Day9.scoreGroups(Day9.parseStream("{{},{}},".toStream)) shouldBe 5
    Day9.scoreGroups(Day9.parseStream("{{{},{},{{}}}},".toStream)) shouldBe 16
    Day9.scoreGroups(Day9.parseStream("{<a>,<a>,<a>,<a>},".toStream)) shouldBe 1
    Day9.scoreGroups(Day9.parseStream("{{<ab>},{<ab>},{<ab>},{<ab>}},".toStream)) shouldBe 9
    Day9.scoreGroups(Day9.parseStream("{{<!!>},{<!!>},{<!!>},{<!!>}},".toStream)) shouldBe 9
    Day9.scoreGroups(Day9.parseStream("{{<a!>},{<a!>},{<a!>},{<ab>}},".toStream)) shouldBe 3

  }

  test("canCountGarbage") {
    Day9.countGarbageChars(Day9.parseStream("<>,".toStream)) shouldBe 0
    Day9.countGarbageChars(Day9.parseStream("<random characters>,".toStream)) shouldBe 17
    Day9.countGarbageChars(Day9.parseStream("<<<<>,".toStream)) shouldBe 3
    Day9.countGarbageChars(Day9.parseStream("<{!>}>,".toStream)) shouldBe 2
    Day9.countGarbageChars(Day9.parseStream("<!!>,".toStream)) shouldBe 0
    Day9.countGarbageChars(Day9.parseStream("<!!!>>,".toStream)) shouldBe 0
    Day9.countGarbageChars(Day9.parseStream("<{o\"i!a,<{i<a>,".toStream)) shouldBe 10
    Day9.countGarbageChars(Day9.parseStream("{{<!!>},{<!!>},{<!!>},{<!!>}},".toStream)) shouldBe 0
    Day9.countGarbageChars(Day9.parseStream("{{<a!>},{<a!>},{<a!>},{<ab>}},".toStream)) shouldBe 17
  }
}
