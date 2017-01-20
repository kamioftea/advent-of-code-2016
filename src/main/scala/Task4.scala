/**
  * Created by jeff on 20/01/2017.
  */
object Task4 {

  case class Room(encryptedName: String, sectorID: Int, checksum: String) {
    lazy val isValid: Boolean = {
      val string = encryptedName
        .replace("-", "")
        .groupBy(c => c)
        .mapValues(_.length)
        .toSeq
        .sortBy {
          case (char, count) => (-count, char)
        }
        .take(5)
        .map(_._1)
        .mkString("")

      string == checksum
    }
  }

  def sumValidRooms(data: String): Int = ???

  def parseRoom(roomCode: String): Option[Room] = {
    val RoomMatcher = "([a-z\\-]+)-(\\d+)\\[([a-z]{5})\\]".r

    roomCode match {
      case RoomMatcher(name, id, check) =>
        Some(Room(name, id.toInt, check))
      case _ =>
        None
    }
  }

}
