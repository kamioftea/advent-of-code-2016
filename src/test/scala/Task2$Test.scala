import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeff on 18/01/2017.
  */
class Task2$Test extends FunSuite with Matchers {

  test("The bathroom codes can be decoded") {
    implicit val stateMachine = Task2.keyPadStateMachine

    // Simpler cases
    Task2.decodeBathroom("") shouldBe "5"
    Task2.decodeBathroom("\n") shouldBe "55"
    Task2.decodeBathroom("U") shouldBe "2"
    Task2.decodeBathroom("UU") shouldBe "2"
    Task2.decodeBathroom("UUDD") shouldBe "8"
    Task2.decodeBathroom("LR") shouldBe "5"
    Task2.decodeBathroom("URDL") shouldBe "5"
    Task2.decodeBathroom("URDDLLUU") shouldBe "1"
    Task2.decodeBathroom("UURRDDDLLLUUU") shouldBe "1"

    // Test Each Transform
    Task2.decodeBathroom(
      ("URDDLL" + "UURDLR" + "RLDDRD" + "RURURU" + "LULULD" + "LDLDRU").mkString("\n")
    ) shouldBe
      "236987" + "412545" + "658899" + "966333" + "221114" + "477785"

    // From Task
    Task2.decodeBathroom(
      "ULL\n" +
        "RRDDD\n" +
        "LURDL\n" +
        "UUUUD") shouldBe "1985"
  }

  test("The diamond bathroom codes can be decoded") {
    implicit val stateMachine = Task2.diamondStateMachine

    // Test Each Transform
    Task2.decodeBathroom(
      ("DLUR" + "ULUR" + "ULUR" + "DRUR" + "DRUR" + "DLDR" + "DLDR" + "DLUL" + "DLUR" + "ULDD" + "RRUU" + "LDRL" + "DULL").mkString("\n")
    ) shouldBe
       "5556" + "2223" + "1111" + "3444" + "8999" + "98CC" + "CBDD" + "DDBA" + "AA67" + "326A" + "BC84" + "3787" + "B765"

    // From Task
    Task2.decodeBathroom(
      "ULL\n" +
        "RRDDD\n" +
        "LURDL\n" +
        "UUUUD") shouldBe "5DB3"
  }
}
