import java.time.LocalDateTime

import scala.annotation.tailrec
import scala.io.Source

object Day5 {

  def countMoves(offsets: Seq[Int]): Int = {
    @tailrec
    def iter(os: Vector[Int], count: Int = 0, position: Int = 0): Int =
      if (!os.isDefinedAt(position)) count
      else iter(
        os.updated(position, os(position) + 1),
        count + 1,
        position = position + os(position)
      )


    iter(offsets.toVector)
  }

  def countMovesWithConvergence(offsets: Seq[Int]): Int = {
    @tailrec
    def iter(os: Vector[Int], count: Int = 0, position: Int = 0): Int =
      if (!os.isDefinedAt(position)) count
      else {
        val offset = os(position)

        iter(
          os.updated(position, if (offset < 3) offset + 1 else offset - 1),
          count + 1,
          position = position + offset
        )
      }


    iter(offsets.toVector)
  }

  def main(args: Array[String]): Unit = {
    println(LocalDateTime.now())
    def input = Source.fromResource("Day5input.txt").getLines().map(_.toInt).toSeq

    println(LocalDateTime.now())
    println(countMoves(input))
    println(LocalDateTime.now())
    println(countMovesWithConvergence(input))
    println(LocalDateTime.now())
  }

}

