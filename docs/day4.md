# Advent of Code 2016 - Day 4

[<< Day 3](./day3) | [Home](./) 

## Checking "Sums"

Today's main theme was string manipulation, along with reusing some of yesterday's
filtering. The data was a list of strings, to be parsed into an encrypted name (more 
on that later), a numeric id, and a check string. The lines that were valid had a 
check string built from the characters in the encrypted name ordered by frequency. 
Ties were broken alphabetically.

The test cases may help clarify that:

```scala
Task4.parseRoom("aaaaa-bbb-z-y-x-123[abxyz]") map (_.isValid) shouldBe Some(true)
Task4.parseRoom("a-b-c-d-e-f-g-h-987[abcde]") map (_.isValid)  shouldBe Some(true)
Task4.parseRoom("not-a-real-room-404[oarel]") map (_.isValid)  shouldBe Some(true)
Task4.parseRoom("totally-real-room-200[decoy]") map (_.isValid)  shouldBe Some(false)
Task4.parseRoom("qzmt-zixmtkozy-ivhz-343[zimth]") map (_.isValid)  shouldBe Some(true)
```

Yet again scala's collections library shines through, and the steps to build the check 
string can just be composed together in a easily understandable way. 

```scala
case class Room(encryptedName: String, sectorID: Int, checksum: String) {
  lazy val calculatedChecksum: String =
    encryptedName
      .replace("-", "")
      .groupBy(identity)
      .mapValues(_.length).toSeq
      .sortBy { case (char, count) => (-count, char) }
      .take(5)
      .map(_._1)
      .mkString("")

  lazy val isValid: Boolean = {
    calculatedChecksum == checksum
  }
}
```

The actual task was to sum the sector ids of the valid room, which could be broken down as

```scala
def sumValidRooms(data: String): Int 
  data.lines
    .flatMap(parseRoom)
    .filter(_.isValid)
    .map(_.sectorID)
    .sum
```

That sucessfully calculated and finished, part two was to decode the "encrypted" name. The
cypher was a simple shift/caesar cypher. Each letter having been rotated round the 
alphabet *sector id* times. 

A sample encrypted room code was provided. The problem didn't seem to have edge cases to hunt
for, so that one sample would likely be enough to test my decoder.

```scala
test("can decode a room name") {
  Task4.parseRoom("qzmt-zixmtkozy-ivhz-343[zimth]") map (_.roomName)  shouldBe Some("very encrypted name")
}
```

Scala's Char.toInt, and Int.toChar, makes swapping between characters and their codes to
do the decoding was pretty easy to write in an understandable way.

```scala
lazy val roomName: String = {
  def rotate(c: Char): Char = {
    val rotation = (sectorID + c.toInt - 'a'.toInt) % 26
    ('a'.toInt + rotation).toChar
  }

  encryptedName.map({
    case c if 'a' <= c && c <= 'z' => rotate(c)
    case _ => ' '
  }).mkString("")
}

```

The solution was the sector id of the room where the north pole objects were stored. The least
precise question so far. I did try for an exact match, but there were no results. So I settled
for a method that would return a list of the rooms that contained a specified string when 
decoded. 

```scala
def findSectorId(data: String, needle: String): Seq[Room] = {
  data.lines
    .flatMap(parseRoom)
    .filter(_.roomName.contains(needle))
    .toSeq
}
```

To get the final result I searched for `north` and just dumped out the resulting id and decoded 
name. Turns out this was easily enough as there was just one matching result: `northpole object
storage`. I will need to consider the need to pass in extra parameters if I get round to writing
a task runner rather than firing up a worksheet or the REPL.

```scala
Task4.findSectorId(input, "north")
  .foreach(r => println(s"${r.roomName} ${r.sectorID}"))
```

[<< Day 3](./day3) | [Home](./) 
