import scala.io.Source

object Day24 {

  case class Component(id: Int, a: Int, b: Int)

  def parseInput(lines: Seq[String]): Map[Int, Set[Component]] = {
    lines
      .map(_.split("/").toSeq)
      .zipWithIndex.collect {
      case ((a +: b +: _), id) => Component(id, a.toInt, b.toInt)
    }
      .foldLeft(Map.empty[Int, Set[Component]]) {
        case (acc, comp) =>
          acc
            .updated(comp.a, acc.getOrElse(comp.a, Set.empty) + comp)
            .updated(comp.b, acc.getOrElse(comp.b, Set.empty) + comp)
      }
  }

  def buildBridges(components: Map[Int, Set[Component]]): Seq[Seq[Component]] = {
    def iter(bridge: Seq[Component], openPort: Int): Seq[Seq[Component]] = {
      bridge +: components.getOrElse(openPort, Seq.empty)
        .filter(!bridge.contains(_))
        .flatMap(c => iter(c +: bridge, if (c.a == openPort) c.b else c.a)).toSeq
    }

    iter(Seq.empty, 0)
  }

  def scoreBridge(bridge: Seq[Component]): Int =
    bridge.map(b => b.a + b.b).sum

  def strongestBridge(components: Map[Int, Set[Component]]):Int =
    buildBridges(components)
    .map(scoreBridge)
    .max

  def longestBridge(components: Map[Int, Set[Component]]):(Int, Int) =
    buildBridges(components)
      .map(b => (b.length, scoreBridge(b)))
      .max

  def main(args: Array[String]): Unit = {
    val components = parseInput(Source.fromResource("day24input.txt").getLines().toSeq)

    println(strongestBridge(components))
    println(longestBridge(components))
  }
}
