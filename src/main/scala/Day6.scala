import scala.annotation.tailrec

object Day6 {
  def countUntilLoop(buckets: Vector[Int]): Int = {
    @tailrec
    def iter(state: Vector[Int], seen: Set[Vector[Int]] = Set.empty, count: Int = 0): Int =
      if(seen.contains(state)) count
      else iter(redistribute(state), seen + state, count + 1)

    iter(buckets)
  }

  def countLoopSize(buckets: Vector[Int]): Int = {
    @tailrec
    def iter(state: Vector[Int], seen: Map[Vector[Int], Int] = Map.empty, count: Int = 0): Int =
      if(seen.isDefinedAt(state)) count - seen(state)
      else iter(redistribute(state), seen.updated(state, count), count + 1)

    iter(buckets)
  }

  def redistribute(buckets: Vector[Int]): Vector[Int] = {
    val max = buckets.max
    val source = buckets.indexOf(max)
    val start = (source + 1) % buckets.length
    val finish = (start + (max % buckets.length)) % buckets.length
    val shouldGetMore: Int => Boolean =
      if (start <= finish) i => i >= start && i < finish
      else i => i >= start || i < finish

    buckets
      .updated(source, 0)
      .zipWithIndex
      .map { case (b, i) => b + (max / buckets.length) + (if (shouldGetMore(i)) 1 else 0) }
  }
}

object RunDay6 extends App {
  println(Day6.countUntilLoop(Vector(0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11)))
  println(Day6.countLoopSize(Vector(0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11)))
}
