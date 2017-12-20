import scala.collection.BitSet
import scala.io.Source

object Day12 {
  private val LineMatcher = "^\\d+ <-> ((?:\\d+(?:, )?)+)$".r

  def parseInput(lines: TraversableOnce[String]): Vector[Set[Int]] =
    lines.toVector.collect {
      case LineMatcher(edges) => edges.split(", ").map(_.toInt).toSet
    }

  def clusterWith(root: Int, edges: Vector[Set[Int]]): BitSet = {
    def iter(linkedEdges: Seq[Int], matches: BitSet): BitSet = linkedEdges match {
      case Nil => matches
      case e +: es if matches.contains(e) => iter(es, matches)
      case e +: es => iter(es ++ edges(e), matches + e)
    }

    iter(edges(root).toSeq, BitSet(root))
  }

def countClusters(edges: Vector[Set[Int]]): Int = {
  def iter(toProcess: Seq[Int], matches: BitSet, count: Int): Int = toProcess match {
    case Nil => count
    case e +: es if matches.contains(e) => iter(es, matches, count)
    case e +: es => iter(es, matches ++ clusterWith(e, edges), count + 1)
  }

  iter(edges.indices, BitSet.empty, 0)
}

  def main(args: Array[String]): Unit = {
    def input = parseInput(Source.fromResource("day12input.txt").getLines())

    println(clusterWith(0, input).size)
    println(countClusters(input))
  }
}
