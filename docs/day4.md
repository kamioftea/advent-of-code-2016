# Advent of Code 2016 - Day 4

[<< Day 3](./day3) | [Home](./) 

## Checking "Sums"

```scala
object Task4 {

  case class Room(encryptedName: String, sectorID: Int, checksum: String)
  {
    lazy val isValid: Boolean = ???
  }

  def sumValidRooms(data: String): Int = ???

  def parseRoom(roomCode: String): Room = ???

}

```

```scala
Task4.findSectorId(input, "north")
  .foreach(r => println(s"${r.roomName} ${r.sectorID}"))
```


[<< Day 3](./day3) | [Home](./) 