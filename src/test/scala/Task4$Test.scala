import Task4._
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by jeff on 20/01/2017.
  */
class Task4$Test extends FunSuite with Matchers{

  test("Can parse a room code") {
    Task4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]") shouldBe Some(Room("aaaaa-bbb-z-y-x", 123, "abxyz"))
    Task4.parseRoom("a-b-c-d-e-f-g-h-987[abcde]") shouldBe Some(Room("a-b-c-d-e-f-g-h", 987, "abcde"))
    Task4.parseRoom("not-a-real-room-404[oarel]") shouldBe Some(Room("not-a-real-room", 404, "oarel"))
    Task4.parseRoom("totally-real-room-200[decoy]") shouldBe Some(Room("totally-real-room", 200, "decoy"))
    Task4.parseRoom("NOTVALID") shouldBe None
  }

  test("Can validate a room checksum") {
    Task4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]") map (_.isValid) shouldBe Some(true)
    Task4.parseRoom("a-b-c-d-e-f-g-h-987[abcde]") map (_.isValid)  shouldBe Some(true)
    Task4.parseRoom("not-a-real-room-404[oarel]") map (_.isValid)  shouldBe Some(true)
    Task4.parseRoom("totally-real-room-200[decoy]") map (_.isValid)  shouldBe Some(false)
    Task4.parseRoom("qzmt-zixmtkozy-ivhz-343[zimth]") map (_.isValid)  shouldBe Some(true)
  }

  test("Can sum valid room sectors") {
    Task4.sumValidRooms(
      "aaaaa-bbb-z-y-x-123[abxyz]\n" +
      "a-b-c-d-e-f-g-h-987[abcde]\n" +
      "not-a-real-room-404[oarel]\n" +
        "totally-real-room-200[decoy]"
    ) shouldBe(123 + 987 + 404)
  }

  test("can decode a room name") {
    Task4.parseRoom("qzmt-zixmtkozy-ivhz-343[zimth]") map (_.roomName)  shouldBe Some("very encrypted name")
  }

}
