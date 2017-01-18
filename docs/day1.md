# Advent of Code 2016 - Day 1

## Initialisation

Today I'm starting from a blank slate, so first of all I need to create a 
project. Luckily most of this is handled by sbt and intellij, so all I need 
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
[info] Loading project definition from C:\Users\Jeff\Code\advent-of-code-2016\project
[info] Set current project to advent-of-code-2016 (in build file:/C:/Users/Jeff/Code/advent-of-code-2016/)
[info] Compiling 1 Scala source to C:\Users\Jeff\Code\advent-of-code-2016\target\scala-2.12\classes...
[info] 'compiler-interface' not yet compiled for Scala 2.12.1. Compiling...
[info]   Compilation completed in 11.229 s
[info] Compiling 1 Scala source to C:\Users\Jeff\Code\advent-of-code-2016\target\scala-2.12\test-classes...
[info] Task1A$Test:
[info] - Strings of instructions should be parsed correctly *** FAILED ***
[info]   scala.NotImplementedError: an implementation is missing
[info]   ...
[info] Run completed in 265 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 0, failed 1, canceled 0, ignored 0, pending 0
[info] *** 1 TEST FAILED ***
[error] Failed tests:
[error]         Task1A$Test
[error] (test:test) sbt.TestsFailedException: Tests unsuccessful
[error] Total time: 17 s, completed 17-Jan-2017 22:24:55
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
