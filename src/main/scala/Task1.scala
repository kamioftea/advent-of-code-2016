/**
  * # Day 1: No Time for a Taxicab
  *
  * Santa's sleigh uses a very high-precision clock to guide its movements, and
  * the clock's oscillator is regulated by stars. Unfortunately, the stars have
  * been stolen... by the Easter Bunny. To save Christmas, Santa needs you to
  * retrieve all fifty stars by December 25th.
  *
  * Collect stars by solving puzzles. Two puzzles will be made available on each
  * day in the advent calendar; the second puzzle is unlocked when you complete
  * the first. Each puzzle grants one star. Good luck!
  *
  * You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
  * unfortunately, is as close as you can get - the instructions on the Easter
  * Bunny Recruiting Document the Elves intercepted start here, and nobody had
  * time to work them out further.
  *
  * The Document indicates that you should start at the given coordinates
  * (where you just landed) and face North. Then, follow the provided sequence:
  * either turn left (L) or right (R) 90 degrees, then walk forward the given
  * number of blocks, ending at a new intersection.
  *
  * There's no time to follow such ridiculous instructions on foot, though, so
  * you take a moment and work out the destination. Given that you can only walk
  * on the street grid of the city, how far is the shortest path to the
  * destination?
  *
  * For example:
  *
  * Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks
  * away.
  * R2, R2, R2 leaves you 2 blocks due South of your starting position, which
  * is 2 blocks away.
  * R5, L5, R5, R3 leaves you 12 blocks away.
  *
  * How many blocks away is Easter Bunny HQ?
  *
  * --- Part Two ---
  *
  * Then, you notice the instructions continue on the back of the Recruiting
  * Document. Easter Bunny HQ is actually at the first location you visit twice.
  *
  * For example, if your instructions are R8, R4, R4, R8, the first location you
  * visit twice is 4 blocks away, due East.
  *
  * How many blocks away is the first location you visit twice?
  *
  * @see http://adventofcode.com/2016/day/1
  */
object Task1 {

  sealed trait Direction

  case object LEFT extends Direction

  case object RIGHT extends Direction

  case class Instruction(turn: Direction, distance: Int)

  def parse(instructions: String): Seq[Instruction] = {
    val Pattern = "(L|R)(\\d+)".r
    Pattern
      .findAllMatchIn(instructions)
      .flatMap {
        case Pattern("L", d) => Some(Instruction(LEFT, d.toInt))
        case Pattern("R", d) => Some(Instruction(RIGHT, d.toInt))
        case _ => None
      }
      .toSeq
  }

  object Compass {
    type Facing = Int

    val NORTH = 0
    val EAST = 1
    val SOUTH = 2
    val WEST = 3

    def turn(currentFacing: Facing, direction: Direction): Facing =
      (currentFacing + (if (direction == LEFT) 3 else 1)) % 4
  }

  import Compass._

  case class Position(x: Int, y: Int, facing: Facing) {
    def move(direction: Direction, distance: Int): Position = {
      val newFacing = turn(facing, direction)

      newFacing match {
        case NORTH => Position(x, y + distance, newFacing)
        case EAST => Position(x + distance, y, newFacing)
        case SOUTH => Position(x, y - distance, newFacing)
        case WEST => Position(x - distance, y, newFacing)
      }
    }

    val coordinates: (Int, Int) = (x, y)
  }

  def finalDistance(instructions: String): Int =
    parse(instructions)
      .foldLeft(Position(0, 0, NORTH)) {
        case (position, instruction) =>
          position.move(instruction.turn, instruction.distance)
      } match {
      case Position(x, y, _) => math.abs(x) + math.abs(y)
    }

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
}
