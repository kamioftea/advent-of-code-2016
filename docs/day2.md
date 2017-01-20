# Advent of Code 2016 - Day 2

[<< Day 1](./day1) | [Home](./) | [Day 3 >>](./day3)

## Going to the Bathroom

Today's challenge is actually quite similar to yesterday's. Again there are a
set of instructions that move a point around a grid. Today however it is limited
to a 3x3 grid, directions that would cause overflow are ignored. The
instructions are in the format `URDL` for one step up, right, down, or left
respectively. Whenever there is a newline the current position (1 to 9) should
be added to the final output, but evaluation of further lines continues from the
current position. The initial position is 5, the center of the grid.

There is only one example given, so I made that into a test. I also added a few
simpler ones of my own, including one for the base case that I can focus on
getting to pass first, then build up the solution from there.

```scala
test("The bathroom codes can be decoded") {
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
  // From Task
  Task2.decodeBathroom(
    "ULL\n" +
      "RRDDD\n" +
      "LURDL\n" +
      "UUUUD") shouldBe "1985"
}
```

So lets implement a basic solution that deals with the base case. Given that
each character in the input is an individual instruction, I'm going to do the
parsing on the fly. As such it seems best to deal with the input as a stream of
characters.

```scala
def decodeBathroom(data: String): String = {
  def iter(characters: Stream[Char], output: String = "", position: Position = 5): String =
    characters match {
      case Stream.Empty => output + position
      case char #:: chars => ???
    }

  iter(data.toStream)
}
```

If I just run the first assertion in the tests, then it passes fine. Include the
second and it falls over horribly due to the unimplemented recursion case.

I'm going to take a slightly different route today. The important features of
today's grid is that it is so small, and the directions are absolute, rather
than relative to the current state. This suggests to me that a finite state
machine would be an appropriate tool for this problem.

First I'll sketch out the types that will shape the solution.

```scala
sealed trait Direction

case object UP extends Direction
case object RIGHT extends Direction
case object DOWN extends Direction
case object LEFT extends Direction

type Position = Int
type Transform = Direction => Position

val stateMachine: Map[Position, Transform] = ???
```

To arrive at a final solution I still need to
 - Build the state machine
 - Parse each character as either a direction or a newline
 - Apply the Transform from the current position for each parsed instruction

There are probably some really clever tricks I can pull to generate the state
machine. But for this problem, hard-coding the grid will be just as quick.

```scala
val stateMachine: Map[Position, Transform] = Map (
  1 -> buildTransform(1, 2, 4, 1),
  2 -> buildTransform(2, 3, 5, 1),
  3 -> buildTransform(3, 3, 6, 2),
  4 -> buildTransform(1, 5, 7, 4),
  5 -> buildTransform(2, 6, 8, 4),
  6 -> buildTransform(3, 6, 9, 5),
  7 -> buildTransform(4, 8, 7, 7),
  8 -> buildTransform(5, 9, 8, 7),
  9 -> buildTransform(6, 9, 9, 8)
)
```

Since hand building this will be prone to error I also added a test that will
try each direction, and push the node to the output after each step.

````scala
// Test Each Transform
Task2.decodeBathroom(
  ("URDDLL" + "UURDLR" + "RLDDRD" + "RURURU" + "LULULD" + "LDLDRU").mkString("\n")
) shouldBe
  "236987" + "412545" + "658899" + "966333" + "221114" + "477785"
````

To decode the stream, I'm going to use a trick from scala pattern matching. If I
implement `unapply(c: Char): Option[Direction]` on the companion object to the
direction trait, I can then use `case Direction(d) => ???` in a match statement
and that will match any of the four direction characters and d will be the
corresponding direction.

```scala
object Direction {
  def unapply(arg: Char): Option[Direction] = arg match {
    case 'U' => Some(UP)
    case 'R' => Some(RIGHT)
    case 'D' => Some(DOWN)
    case 'L' => Some(LEFT)
    case _ => None
  }
}
```

With these in place the recursive cases can be built for
- A newline => add the current position to the output
- A character that matches a direction => apply the transformation for that   
  direction
- Anything else => ignore and carry on

```scala
def decodeBathroom(data: String): String = {
  @tailrec
  def iter(characters: Stream[Char], output: String = "", position: Position = 5): String =
    characters match {
      case Stream.Empty =>
        output + position
      case '\n' #:: chars =>
        iter(chars, output + position, position)
      case Direction(d) #:: chars =>
        iter(chars, output, stateMachine(position)(d))
      case _ #:: chars =>
        iter(chars, output, position)
    }
  iter(data.toStream)
}
```

For which all the tests passed. Also for my piece of mind I purposefully broke
the state machine. The test that tried all the possible transformations pointed
exactly to the transformation that was broken when I changed going right from 9
to incorrectly result in position 8.

```
"236987412545658899[85]6333221114477785" was not equal to "236987412545658899[96]6333221114477785"
```

## Task 2 - Like that, but badly designed

For task 2, the grid is changed to a diamond shape with four extra keys:

    -------------
    |     1     |
    |   2 3 4   |
    | 5 6 7 8 9 |
    |   A B C   |
    |     D     |
    -------------

So the state machine plan seems to have been the right choice. I have a couple
refactorings to do:

 - A Position should be a Char, not an Int
 - I need to be able to specify the state machine to use when running the
   decode.

I then need to write the new state machine and add two tests, the one from the
example and a full route check.

The first refactor is simply changing the type alias of Position, and fixing the
type errors. The second uses another powerful feature of scala: implicit
parameters. If I augment the signature of decodeBathroom to

```
def decodeBathroom(data: String)(implicit stateMachine: Map[Position,Transform]): String
```

All I need to do to get my initial set of tests passing again is provide an
implicit state machine once:

```scala
test("The bathroom codes can be decoded") {
  implicit val stateMachine = Task2.keyPadStateMachine

  // Simpler cases
  Task2.decodeBathroom("") shouldBe "5"
  Task2.decodeBathroom("\n") shouldBe "55"
  // ...
}
```

The new state machine and tests are then

```scala
val diamondStateMachine: Map[Position, Transform] = Map(
  '1' -> buildTransform('1', '1', '3', '1'),
  '2' -> buildTransform('2', '3', '6', '2'),
  '3' -> buildTransform('1', '4', '7', '2'),
  '4' -> buildTransform('4', '4', '8', '3'),
  '5' -> buildTransform('5', '6', '5', '5'),
  '6' -> buildTransform('2', '7', 'A', '5'),
  '7' -> buildTransform('3', '8', 'B', '6'),
  '8' -> buildTransform('4', '9', 'C', '7'),
  '9' -> buildTransform('9', '9', '9', '8'),
  'A' -> buildTransform('6', 'B', 'A', 'A'),
  'B' -> buildTransform('7', 'C', 'D', 'A'),
  'C' -> buildTransform('8', 'C', 'C', 'B'),
  'D' -> buildTransform('B', 'D', 'D', 'D')
)

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
```

Finally I can now apply the updated function to the test data, and it gives the
correct answer.

[<< Day 1](./day1) [Home](./) [Day 3 >>](./day3)
