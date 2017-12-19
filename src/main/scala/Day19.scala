import scala.io.Source

object Day19 {
  def findStart(grid: Vector[String]): Int = {
    grid(0).indexOf('|')
}

  def findPath(grid: Vector[String]): (String, Int) = {
    def canTravel(x: Int, y: Int): Boolean =
      grid.isDefinedAt(y) &&
        grid(y).isDefinedAt(x) &&
        grid(y)(x) != ' '

    def init(x: Int, y: Int, path: String, dx: Int, dy: Int, count: Int): (String, Int) =
      if (!canTravel(x, y))
        (path, count)
      else grid(y)(x) match {
        case '|' | '-' => init(x + dx, y + dy, path, dx, dy, count + 1)
        case '+' =>
          if (canTravel(x + dx, y + dy)) init(x + dx, y + dy, path, dx, dy, count + 1)
          else if (canTravel(x + dy, y + dx)) init(x + dy, y + dx, path, dy, dx, count + 1)
          else if (canTravel(x - dy, y - dx)) init(x - dy, y - dx, path, -dy, -dx, count + 1)
          else (path, count)
        case c => init(x + dx, y + dy, path + c, dx, dy, count + 1)
      }

    init(findStart(grid), 0, "", 0, 1, 0)
  }

  def main(args: Array[String]): Unit = {
    def grid = Source.fromResource("day19input.txt").getLines().toVector

    println(findPath(grid))
  }
}
