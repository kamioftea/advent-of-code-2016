object Day10 {

  def hashRound(lengths: List[Int],
                hash: Vector[Int],
                position: Int = 0,
                skipSize: Int = 0): (Vector[Int], Int, Int) = {
    lengths match {
      case Nil => (hash, position, skipSize)
      case h :: t if position + h < hash.length =>
        val (pre, rest) = hash.splitAt(position)
        val (mid, post) = rest.splitAt(h)

        hashRound(
          t,
          pre ++ mid.reverse ++ post,
          (position + h + skipSize) % hash.length,
          skipSize + 1
        )

      case h :: t =>
        val (rest, end) = hash.splitAt(position)
        val (start, mid) = rest.splitAt((position + h) % hash.length)
        val (rEnd, rStart) = (end ++ start).reverse.splitAt(end.length)

        hashRound(
          t,
          rStart ++ mid ++ rEnd,
          (position + h + skipSize) % hash.length,
          skipSize + 1
        )
    }
  }

  def hash(lengths: List[Int], listSize: Int, position: Int = 0, skipSize: Int = 0): Vector[Int] = {
    hashRound(lengths, Vector.range(0, listSize))._1
  }

  def knotHash(input: String, listSize: Int = 256): String = {
    val lengths = input.toList.map(_.toInt) ++ List(17, 31, 73, 47, 23)
    val (sparseHash, _, _) =
      (0 until 64)
        .foldLeft((Vector.range(0, listSize), 0, 0)) {
          case ((h, p, s), _) => hashRound(lengths, h, p, s)
        }

    sparseHash
      .grouped(16)
      .map(_.reduce(_ ^ _))
      .map("%02x".format(_))
      .mkString
  }

  def main(args: Array[String]): Unit = {
    val input = "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63"

    val output = hash(input.split(",").map(_.toInt).toList, 256)
    println(output(0) * output(1))

    println(knotHash(input))
  }

}
