import Day25._
import org.scalatest.{FunSuite, Matchers}

class Day25Test extends FunSuite with Matchers{

  test("can parse input") {
    parseInput("""Begin in state A.
      |Perform a diagnostic checksum after 6 steps.
      |
      |In state A:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state B.
      |  If the current value is 1:
      |    - Write the value 0.
      |    - Move one slot to the left.
      |    - Continue with state B.
      |
      |In state B:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the left.
      |    - Continue with state A.
      |  If the current value is 1:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state A.""".stripMargin.lines.toSeq) should be
    Day25.Program(
      'A',
      6,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )
  }

  test("can get dignostic checksum") {
    debugChecksum(Day25.Program(
      'A',
      6,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 3

    debugChecksum(Day25.Program(
      'A',
      5,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 3

    debugChecksum(Day25.Program(
      'A',
      4,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 2

    debugChecksum(Day25.Program(
      'A',
      3,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 1

    debugChecksum(Day25.Program(
      'A',
      2,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 2

    debugChecksum(Day25.Program(
      'A',
      1,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 1

    debugChecksum(Day25.Program(
      'A',
      0,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 0
  }

}
