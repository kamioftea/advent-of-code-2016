import Task5._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by jeff on 20/01/2017.
  */
class Task4$Test extends FunSuite with Matchers{

  test("Can find example door code") {
    Task5.buildDoorCode("abc") shouldBe "18f47a30"
  }

}
