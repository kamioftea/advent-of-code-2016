object Day15 {

  def buildIterator(start: Int, factor: Long): Iterator[Int] =
    Iterator.iterate(start)(i => ((i * factor) % 2147483647).toInt).drop(1)

  def buildLSBChecker(numBits: Int): ((Int, Int)) => Boolean = {
    val mask = (1 << numBits) - 1

    (p: (Int, Int)) => (p._1 & mask) == (p._2 & mask)
  }

  def countMatches(a: Iterator[Int], b: Iterator[Int], iterations: Int): Int = {
    a.take(iterations)
      .zip(b.take(iterations))
      .count(buildLSBChecker(16))
  }

  def filterIterator(start: Int, factor: Long, divisor: Int): Iterator[Int] =
    buildIterator(start, factor).filter(_ % divisor == 0)

  def main(args: Array[String]): Unit = {

    println(countMatches(
      buildIterator(591, 16807),
      buildIterator(393, 48271),
      40000000
    ))

    println(countMatches(
      filterIterator(591, 16807, 4),
      filterIterator(393, 48271, 8),
      5000000
    ))
  }
}
