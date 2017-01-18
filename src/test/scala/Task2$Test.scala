import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Jeff on 18/01/2017.
  */
class Task2$Test extends FunSuite with Matchers {

  test("The bathroom codes can be decoded") {
    // Simpler cases
    Task2.decodeBathroom("") shouldBe "5"
    Task2.decodeBathroom("U") shouldBe "2"
    Task2.decodeBathroom("UU") shouldBe "2"
    Task2.decodeBathroom("UUDD") shouldBe "8"
    Task2.decodeBathroom("LR") shouldBe "5"
    Task2.decodeBathroom("URDL") shouldBe "5"

    // From Task
    Task2.decodeBathroom(
      "ULL\n" +
        "RRDDD\n" +
        "LURDL\n" +
        "UUUUD") shouldBe "1985"

  }

}
