import scala.collection.BitSet

object Day14 {

  def countBits(hexString: String): Int =
    hexString
      .grouped(4).map(s => Integer.parseInt(s, 16).toBinaryString)
      .mkString("")
      .count(_ == '1')

  def countUsed(key: String): Int = {
    (0 to 127)
      .map(i => Day10.knotHash(s"$key-$i"))
      .map(countBits)
      .sum
  }

  def drawBits(hexString: String): String =
    hexString
      .grouped(4).map(s => Integer.parseInt(s, 16).toBinaryString.formatted("%16s").replaceAll(" ", "0"))
      .mkString("")

  def countRegions(key: String): Int = {
    val matrix = Vector.iterate(0, 128)(_ + 1).map(i => drawBits(Day10.knotHash(s"$key-$i")))
    val edgesWithMask: Vector[(Set[Int], Boolean)] = (for(x <- 0 to 127; y <- 0 to 127) yield {
      (Set((-1, 0), (0, -1), (1, 0), (0,1)).collect {
        case (dx, dy) if matrix.isDefinedAt(x + dx) && matrix(x + dx).isDefinedAt(y + dy) && matrix(x + dx)(y + dy) == matrix(x)(y)
          => y + dy + 128 * (x + dx)
      }, matrix(x)(y) == '1')
    }).toVector

    def clusterWith(root: Int, edges: Vector[Set[Int]]): BitSet = {
      def iter(linkedEdges: Seq[Int], matches: BitSet): BitSet = linkedEdges match {
        case Nil => matches
        case e +: es if matches.contains(e) => iter(es, matches)
        case e +: es => iter(es ++ edges(e), matches + e)
      }

      iter(edges(root).toSeq, BitSet(root))
    }

    def countClusters(edges: Vector[Set[Int]], mask: Vector[Boolean]): Int = {
      def iter(toProcess: Seq[Int], matches: BitSet, count: Int): Int = toProcess match {
        case Nil => count
        case e +: es if matches.contains(e) => iter(es, matches, count)
        case e +: es => iter(es, matches ++ clusterWith(e, edges), count + 1)
      }

      iter(edges.indices.filter(e => mask(e)), BitSet.empty, 0)
    }

    val edges = edgesWithMask.map(_._1)
    val mask = edgesWithMask.map(_._2)

    countClusters(edges, mask)
  }

  def main(args: Array[String]): Unit = {

    println(countUsed("xlqgujun"))
    println(countRegions("xlqgujun"))
  }
}
