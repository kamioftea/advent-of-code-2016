import scala.io.Source

object Day13 {

  case class Layer(depth: Int, range: Int)

  private val LineMatcher = "(\\d+): (\\d+)".r

  def parseLines(lines: TraversableOnce[String]): Seq[Layer] =
    lines.toSeq.collect { case LineMatcher(d, r) => Layer(d.toInt, r.toInt) }

  def calcTripSeverity(layers: Seq[Layer]): Int =
    layers
      .filter(l => l.depth % (2 * l.range - 2) == 0)
      .map(l => l.depth * l.range)
      .sum

  def calcSafeTrip(layers: Seq[Layer]): Int =
    Stream.from(0)
      .filter(offset =>
        layers.forall(l =>
          (l.depth + offset) % (2 * l.range - 2) != 0
        )
      )
      .head

  def main(args: Array[String]): Unit = {
    def input = parseLines(Source.fromResource("day13input.txt").getLines())

    println(calcTripSeverity(input))

    println(calcSafeTrip(input))
  }
}
