import Task1._
import org.scalatest.{FunSuite, Matchers}

class Task1$Test extends FunSuite with Matchers {

  test("Strings of instructions should be parsed correctly") {
    Task1.parse("R2, L3") should be(Seq(
      Instruction(RIGHT, 2),
      Instruction(LEFT, 3)
    ))

    Task1.parse("R2, R2, R2") should be(Seq(
      Instruction(RIGHT, 2),
      Instruction(RIGHT, 2),
      Instruction(RIGHT, 2)
    ))

    Task1.parse("R5, L5, R5, R3") should be(Seq(
      Instruction(RIGHT, 5),
      Instruction(LEFT, 5),
      Instruction(RIGHT, 5),
      Instruction(RIGHT, 3)
    ))
  }

  test("Turning left or right should result in the correct direction") {
    import Task1.Compass._
    turn(NORTH, RIGHT) shouldBe EAST
    turn(NORTH, LEFT) shouldBe WEST
    turn(turn(NORTH, LEFT), RIGHT) shouldBe NORTH
    turn(turn(turn(turn(NORTH, LEFT), LEFT), LEFT), LEFT) shouldBe NORTH
  }

  test("A sequence of instructions should give a final distance from the origin") {
    Task1.finalDistance("R2, L3") shouldBe 5
    Task1.finalDistance("R2, R2, R2") shouldBe 2
    Task1.finalDistance("R5, L5, R5, R3") shouldBe 12
  }

  test("A sequence of instructions should give a final distance from the " +
    "origin to the first repeated position") {
    // From Task
    Task1.distanceToFirstRepeat("R8, R4, R4, R8") shouldBe Some(4)
    // Extras
    Task1.distanceToFirstRepeat("R2, R2, R2") shouldBe None
    Task1.distanceToFirstRepeat("R2, R2, R2, R2") shouldBe Some(0)
    Task1.distanceToFirstRepeat("R8, R2, R8, L2, L4, L8") shouldBe Some(6)
    Task1.distanceToFirstRepeat("R6, R2, R8, R4, R6, R8") shouldBe Some(4)
  }

}
