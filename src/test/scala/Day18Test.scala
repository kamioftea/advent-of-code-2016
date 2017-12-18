import Day18._
import org.scalatest.{FunSuite, Matchers}

class Day18Test extends FunSuite with Matchers {

  test("testInstFromLine") {
    instFromLine("set a 1") shouldBe Instruction("set", 'a', Literal(1))
    instFromLine("add a 2") shouldBe Instruction("add", 'a', Literal(2))
    instFromLine("mul a a") shouldBe Instruction("mul", 'a', Ref('a'))
    instFromLine("mod a 5") shouldBe Instruction("mod", 'a', Literal(5))
    instFromLine("snd a") shouldBe Instruction("snd", 'a', EmptyValue)
    instFromLine("set a 0") shouldBe Instruction("set", 'a', Literal(0))
    instFromLine("rcv a") shouldBe Instruction("rcv", 'a', EmptyValue)
    instFromLine("jgz a -1") shouldBe Instruction("jgz", 'a', Literal(-1))
    instFromLine("set a 1") shouldBe Instruction("set", 'a', Literal(1))
    instFromLine("jgz a -2") shouldBe Instruction("jgz", 'a', Literal(-2))
  }

  test("testParseLines") {
    parseLines(
      """set a 1
        |add a 2
        |mul a a
        |mod a 5
        |snd a
        |set a 0
        |rcv a
        |jgz a -1
        |set a 1
        |jgz a -2""".stripMargin.lines
    ) shouldBe Vector(
      Instruction("set", 'a', Literal(1)),
      Instruction("add", 'a', Literal(2)),
      Instruction("mul", 'a', Ref('a')),
      Instruction("mod", 'a', Literal(5)),
      Instruction("snd", 'a', EmptyValue),
      Instruction("set", 'a', Literal(0)),
      Instruction("rcv", 'a', EmptyValue),
      Instruction("jgz", 'a', Literal(-1)),
      Instruction("set", 'a', Literal(1)),
      Instruction("jgz", 'a', Literal(-2))
    )
  }

  test("findRcv") {
    getRcv(Vector(
      Instruction("set", 'a', Literal(1)),
      Instruction("add", 'a', Literal(2)),
      Instruction("mul", 'a', Ref('a')),
      Instruction("mod", 'a', Literal(5)),
      Instruction("snd", 'a', EmptyValue),
      Instruction("set", 'a', Literal(0)),
      Instruction("rcv", 'a', EmptyValue),
      Instruction("jgz", 'a', Literal(-1)),
      Instruction("set", 'a', Literal(1)),
      Instruction("jgz", 'a', Literal(-2))
    )) shouldBe Some(4)
  }

}
