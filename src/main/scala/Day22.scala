import scala.io.Source

object Day22 {

  def parseInput(lines: Seq[String]): Map[Int, Map[Int, Char]] =
    lines.zipWithIndex
        .map {
          case (line, index) =>
            index - (lines.size / 2) ->
              line.zipWithIndex.map {
                case (c, i) => i - (line.length / 2) -> c
              }.toMap
        }.toMap

  def countInfections(grid: Map[Int, Map[Int, Char]],
                      iterations: Int,
                      infectionStrategy: PartialFunction[(Int, Int, Char), (Int, Int, Char)]): Int = {
    def iter(grid: Map[Int, Map[Int, Char]],
             x: Int, y: Int,
             dx:Int, dy: Int,
             count: Int, infectionCount: Int): Int = {
      if(count == iterations) return infectionCount

      val status = grid.getOrElse(y, Map.empty).getOrElse(x, '.')
      val (ndx, ndy, nStatus) = infectionStrategy((dx, dy, status))
      val nICount = if(nStatus == '#') infectionCount + 1 else infectionCount
      iter(
        grid.updated(y, grid.getOrElse(y, Map.empty).updated(x, nStatus)),
        x + ndx, y + ndy,
        ndx, ndy,
        count + 1, nICount
      )
    }

    iter(grid, 0, 0, 0, -1, 0, 0)
  }

  val basicStrategy: PartialFunction[(Int, Int, Char), (Int, Int, Char)] = {
    case (dx, dy, '.') => (dy, -dx, '#')
    case (dx, dy, '#') => (-dy, dx, '.')
  }

  val evolvedStrategy: PartialFunction[(Int, Int, Char), (Int, Int, Char)] = {
    case (dx, dy, '.') => (dy, -dx, 'W')
    case (dx, dy, 'W') => (dx, dy, '#')
    case (dx, dy, '#') => (-dy, dx, 'F')
    case (dx, dy, 'F') => (-dx, -dy, '.')
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput(Source.fromResource("day22input.txt").getLines().toSeq)

    println(countInfections(input, 10000, basicStrategy))
    println(countInfections(input, 10000000, evolvedStrategy))
  }

}
