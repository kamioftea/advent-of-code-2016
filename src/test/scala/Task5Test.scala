import Task5._
import org.scalatest.{FunSuite, Matchers}

class Task5Test extends FunSuite with Matchers {
  test("can md5")
  {
    md5("test") shouldBe "098f6bcd4621d373cade4e832627b4f6"
    md5("testing123") shouldBe "7f2ababa423061c509f4923dd04b6cf1"
    md5("abc5017308") should startWith("000008")
  }

  test("find password") {
    findPassword("abc") shouldBe "18f47a30"
  }

  test("find unordered password") {
    findUnorderedPassword("abc") shouldBe "05ace8e3"
  }
}
