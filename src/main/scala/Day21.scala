import scala.io.Source

object Day21 {

  implicit class MatrixOps[T](m: Vector[Vector[T]]) {
    def flip: Vector[Vector[T]] = m.map(_.reverse)
    def rot: Vector[Vector[T]] = m.transpose.flip
  }

  def explode(base: Vector[Vector[Char]]): Seq[Vector[Vector[Char]]] = {
    Seq(
      base,
      base.flip,
      base.rot,
      base.rot.flip,
      base.rot.rot,
      base.rot.rot.flip,
      base.rot.rot.rot,
      base.rot.rot.rot.flip
    )
  }

  def parseInput(lines: TraversableOnce[String]): Map[Vector[Vector[Char]], Vector[Vector[Char]]] = {
    lines.flatMap(
      l => {
        val (base +: result +: _) =
          l.split(" => ")
            .map(r => r.split("/").toVector.map(_.toVector))
            .toSeq

        explode(base).map(_ -> result)
      }
    ).toMap
  }

  def expand(grid: Vector[Vector[Char]],
             book: Map[Vector[Vector[Char]], Vector[Vector[Char]]],
             rounds: Int): Vector[Vector[Char]] = {

    def _doExpand(i: Int): Vector[Vector[Char]] = {
      val newWidth = (grid(0).length / i) * (i + 1)
      (for {
        x <- 0 until grid(0).length / i
        y <- 0 until grid(0).length / i
        (img, a) <- book(
          (0 until i).map(j =>
            grid((i * x) + j).slice(i * y, (i * y) + i)
          ).toVector
        ).zipWithIndex
        (ch, b) <- img.zipWithIndex
      } yield (x, y, ch, a, b)).foldLeft(Vector.fill(newWidth, newWidth)(' ')) {
        case (acc, (x, y, ch, a, b)) =>
          acc.updated(
            x * (i + 1) + a,
            acc((x * (i + 1)) + a).updated(
              y * (i + 1) + b,
              ch
            )
          )
      }
    }


    if (rounds == 0) grid
    else {
      if (grid(0).length % 2 == 0)
        expand(_doExpand(2), book, rounds - 1)
      else
        expand(_doExpand(3), book, rounds - 1)
    }
  }

  def countPixels(grid: Vector[Vector[Char]],
                  book: Map[Vector[Vector[Char]], Vector[Vector[Char]]],
                  rounds: Int): Int = {
    expand(grid, book, rounds).map(_.count(_ == '#')).sum
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput(Source.fromResource("day21input.txt").getLines())

    val base = Vector(
      ".#.",
      "..#",
      "###"
    ).map(_.toVector)

    println(countPixels(base, input, 5))
    println(countPixels(base, input, 18))
  }
}
