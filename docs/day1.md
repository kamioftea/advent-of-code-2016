# Advent of Code 2016 - Day 1

## Initialisation

Today I'm starting from a blank slate, so first of all I need to create a
project. Luckily most of this is handled by sbt and IntelliJ, so all I need
to do is update the build.sbt with the [ScalaTest](http://scalatest.org)
dependencies as that is the test framework I'm planning to use.

```scala
name := "advent-of-code-2016"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
```

There will be more to do later to provide a way to run the daily tasks with
the provided input files, but this allows me to get started.

## Task One: Determine distance on a grid

With that in place I can get started on the first task.

> Given a sequence instructions in the format `R1, L2, R3, ...` meaning turn
> 90&deg; right then forward 1, turn 90&deg; left then go forward 2, and so on.
> Determine the distance of the final position from your starting location
> using [Taxi Cab Geometry](https://en.wikipedia.org/wiki/Taxicab_geometry).

Firstly some types that will be used to describe the instructions, and a
placeholder for a method to parse the instruction input.

```scala
sealed trait Direction

case object LEFT extends Direction
case object RIGHT extends Direction

case class Instruction(turn: Direction, distance: Int)

def parse(data: String): Seq[Instruction] = ???
```

Allowing me to create my first test.

```scala
import Task1._
import org.scalatest.FunSuite

class Task1$Test extends FunSuite {

  test("Strings of instructions should be parsed correctly") {
    assertResult(Seq(
      Instruction(RIGHT, 2),
      Instruction(LEFT, 3)
    )) {
      Task1A.parse("R2, L3")
    }
    assertResult(Seq(
      Instruction(RIGHT, 2),
      Instruction(RIGHT, 2),
      Instruction(RIGHT, 2)
    )) {
      Task1A.parse("R2, R2, R2")
    }
    assertResult(Seq(
      Instruction(RIGHT, 5),
      Instruction(LEFT, 5),
      Instruction(RIGHT, 5),
      Instruction(RIGHT, 3)
    )) {
      Task1A.parse("R5, L5, R5, R3")
    }
  }
}
```

Which dutifully fails because I haven't implemented anything yet.

```
Loading project definition from C:\Users\Jeff\Code\advent-of-code-2016\project
Set current project to advent-of-code-2016 (in build file:/C:/Users/Jeff/Code/advent-of-code-2016/)
Compiling 1 Scala source to C:\Users\Jeff\Code\advent-of-code-2016\target\scala-2.12\classes...
'compiler-interface' not yet compiled for Scala 2.12.1. Compiling...
  Compilation completed in 11.229 s
Compiling 1 Scala source to C:\Users\Jeff\Code\advent-of-code-2016\target\scala-2.12\test-classes...
Task1A$Test:
- Strings of instructions should be parsed correctly *** FAILED ***
  scala.NotImplementedError: an implementation is missing
  ...
Run completed in 265 milliseconds.
Total number of tests run: 1
Suites: completed 1, aborted 0
Tests: succeeded 0, failed 1, canceled 0, ignored 0, pending 0
*** 1 TEST FAILED ***
```

I started with an implementation that utilises the power of Scala's
Regex implementation combined with its pattern matching.

```scala
def parse(instructions: String): Seq[Instruction] = {
  val Pattern = "(L|R)\\d+".r
  Pattern
    .findAllMatchIn(instructions)
    .flatMap {
      case Pattern("L", d) => Some(Instruction(LEFT, d.toInt))
      case Pattern("R", d) => Some(Instruction(RIGHT, d.toInt))
      case _ => None
    }
    .toSeq
  }
```

Which was pretty close but there is one bug.

```
Task1A$Test:
- Strings of instructions should be parsed correctly *** FAILED ***
  Stream() was not equal to List(Instruction(RIGHT,2), Instruction(LEFT,3)) (Task1A$Test.scala:7)
Run completed in 304 milliseconds.
Total number of tests run: 1
Suites: completed 1, aborted 0
Tests: succeeded 0, failed 1, canceled 0, ignored 0, pending 0
*** 1 TEST FAILED ***
```

It actually took me a while to work out what was going on. I initially thought
that the problem was because I was comparing a List (the default concrete
implementation for Seq literals) with a Stream (what you get from Iterator
.toSeq) so the test failed because of the type mismatch. Having found some
documentation that suggested that, no, ScalaTest is clever enough to read as
much of the stream as needed to determine if they are equivalent, and the
error was that I was returning an empty Stream. Some breakpoints and
step-debugging later I realised that the regex was wrong and should have been
`"(L|R)(\\d+)"` to properly pattern match.

```
Task1$Test:
- Strings of instructions should be parsed correctly
Run completed in 270 milliseconds.
Total number of tests run: 1
Suites: completed 1, aborted 0
Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
All tests passed.
```

In hunting for how to fix the non-existent type error, I also found what I
think is a cleaner way to write the tests.

```scala
import Task1._
import org.scalatest.{FunSuite, Matchers}

class Task1A$Test extends FunSuite with Matchers {

  test("Strings of instructions should be parsed correctly") {
    Task1A.parse("R2, L3") should be(Seq(
      Instruction(RIGHT, 2),
      Instruction(LEFT, 3)
    ))
    // ...
  }
}
```

With the data in a useful format I can now solve the actual question. The
task description provided some test cases so I used those.

```
test("A sequence of instructions should give a final distance from the origin") {
  Task1.finalDistance("R2, L3") shouldBe 5
  Task1.finalDistance("R2, R2, R2") shouldBe 2
  Task1.finalDistance("R5, L5, R5, R3") shouldBe 12
}
```

To calculate the final distance we need to keep track of how far we have
moved on the North/South(Y) axis and also East/West Axis. Summing those two
values then gives the distance in taxi geometry.

Firstly, note that each turn is going to be one step forward or backwards
round the compass. This can be encoded using simple modulus arithmetic.

```scala
object Compass {
  type Facing = Int

  val NORTH = 0
  val EAST = 1
  val SOUTH = 2
  val WEST = 3

  def turn(currentFacing: Facing, direction: Direction): Facing =
    (currentFacing + (if (direction == LEFT) 3 else 1)) % 4
}
```

A new position can be calculated using a facing and a distance

```scala
case class Position(x: Int, y: Int, facing: Facing)
{
  def move(direction: Direction, distance: Int): Position = {
    val newFacing = turn(facing, direction)

    newFacing match {
      case NORTH => Position(x, y + distance, newFacing)
      case EAST => Position(x + distance, y, newFacing)
      case SOUTH => Position(x, y - distance, newFacing)
      case WEST => Position(x - distance, y, newFacing)
    }
  }
}
```

We can then use foldLeft to consume the instructions, updating the current
position as we go. Then extract the data we need from the final position.

```scala
def finalDistance(instructions: String): Int =
  parse(instructions)
    .foldLeft(Position(0, 0, NORTH)) {
      case (position, instruction) =>
        position.move(instruction.turn, instruction.distance)
    } match {
    case Position(x, y, _) => math.abs(x) + math.abs(y)
  }
```

Whilst I plan to write a command line interface to run the task with the
provided data, I'll set that up another day. For now I'll just run my code
from a Scala worksheet.

````scala
import Task1._

var input = "R3, L5, R2, ..."

finalDistance(input)
````

Which gave the correct answer.

## Task 2 - Actually, just the first time you cross your path

So I added a stub method and the one example from the task as a test

```scala
test("A sequence of instructions should give a final distance from the " +
  "origin to the first repeated position") {

  Task1.distanceToFirstRepeat("R8, R4, R4, R8") shouldBe 4
}
```

I then went and hacked out a rehash of my first solution that kept track of the
points visited so far in a set and exited out early when the first duplicated
position was found, ignoring facing. Given that it would be possible to never
cross your path, I updated the return type to `Option[Int]` and if the
instructions are exhausted then None is returned. It was written using a custom
recursive function rather than a fold or other higher order function. I think
this makes it easier to see what the method is doing and makes the various cases
more explicit. It also makes it simple to halt execution at the point the first
duplicate is found. As the tasks go on I may look into bringing
[Akka](http://akka.io/) into my toolkit as it makes breaking down these kinds of
problems pretty easy. That will require some initial work on some scaffolding
around running the tasks. Since I plan to add something like that at some point,
I'll look int to Akka after that.

```scala
def distanceToFirstRepeat(data: String): Option[Int] = {
  def iter(pos: Position, trail: Set[(Int, Int)], instructions:
  List[Instruction]): Option[Position] =
    instructions match {
      case Nil => None
      case instruction :: tail =>
        val newPos = pos.move(instruction.turn, instruction.distance)
        if (trail.contains(newPos.coordinates))
          Some(newPos)
        else
          iter(newPos, trail + pos.coordinates, tail)
    }

  iter(Position(0, 0, NORTH), Set(), parse(data).toList)
    .map(p => math.abs(p.x) + math.abs(p.y))
}
```

Which failed.

The solution was too simple and didn't take into account points where the path
crossed itself, but didn't turn at that intersection. This is what was happening
in the task's example. Realising this I first decided to add a few more tests.
One where the path didn't cross, one where the path did in fact first get to the
same place at an endpoint of a previous instruction, and two where an
instruction caused both the first and second crossing to ensure I got the
ordering right if that happened. I also corrected the expected values to Options

```scala
// From Task
Task1.distanceToFirstRepeat("R8, R4, R4, R8") shouldBe Some(4)
// Extras
Task1.distanceToFirstRepeat("R2, R2, R2") shouldBe None
Task1.distanceToFirstRepeat("R2, R2, R2, R2") shouldBe Some(0)
Task1.distanceToFirstRepeat("R8, R2, R8, L2, L4, L8") shouldBe Some(6)
Task1.distanceToFirstRepeat("R6, R2, R8, R4, R6, R8") shouldBe Some(4)
```

To allow me to reuse as much as possible, I decided to keep the trail building,
but do so one step at a time. I considered changing the grid representation to
some form of matrix of Booleans, but it didn't seem worth refactoring that much
when the trail idea could still work.

Firstly there was some refactoring to allow moving along a specific facing
without turning first.

```scala
def move(direction: Direction, distance: Int): Position =
  move(turn(facing, direction), distance)

def move(newFacing: Facing, distance: Int): Position =
  newFacing match {
    case NORTH => Position(x, y + distance, newFacing)
    case EAST => Position(x + distance, y, newFacing)
    case SOUTH => Position(x, y - distance, newFacing)
    case WEST => Position(x - distance, y, newFacing)
  }
```

I also cleaned up the (Int, Int) pairs into a Coordinates case class to make
what they were clearer.

This was then used to build a helper function that walked a number of steps from
a given position, but returned the points along the path as well as the final
position.

```scala
@tailrec
def moveStepwise(position: Position,
                 facing: Facing,
                 distance: Int,
                 path: Seq[Coordinates] = Seq()): (Position, Seq[Coordinates]) =
  distance match {
    case 0 => (position, path)
    case i if i < 0 =>
      moveStepwise(position, turn(turn(facing, RIGHT), RIGHT), -distance, path)
    case _ =>
      val newPos = position.move(facing, 1)
      moveStepwise(newPos, facing, distance - 1, path :+ newPos.coordinates)
  }
```

From this I needed to analyse if the path had crossed itself, and this was also
a good place to keep the set of previous coordinates up to date. So I wrote it
as a function that could either return the first time the path crossed, or the
updated trail if it didn't.

```scala
@tailrec
def firstIntersectionOrExtendedTrail(trail: Set[Coordinates], path: List[Coordinates]): Either[Coordinates, Set[Coordinates]] = {
  path match {
    case Nil => Right(trail)
    case h :: _ if trail.contains(h) => Left(h)
    case h :: t => firstIntersectionOrExtendedTrail(trail + h, t)
  }
```

From this I could refactor my first attempt to include the extra coordinates.

```scala
@tailrec
def iter(pos: Position, trail: Set[Coordinates], instructions:
List[Instruction]): Option[Coordinates] =
  instructions match {
    case Nil => None
    case instruction :: tail =>
      val (newPos, path) = moveStepwise(pos, turn(pos.facing, instruction.direction), instruction.distance)
      firstIntersectionOrExtendedTrail(trail, path.toList) match {
        case Left(coordinates) => Some(coordinates)
        case Right(newTrail) => iter(newPos, newTrail, tail)
      }
  }
iter(Position(0, 0, NORTH), Set(Coordinates(0,0)), parse(data).toList)
  .map(p => math.abs(p.x) + math.abs(p.y))
```

This passed all tests, and when run with the actual data gave the correct
answer.
