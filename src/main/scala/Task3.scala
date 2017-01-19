import scala.util.matching.Regex

/**
  * Created by jeff on 19/01/2017.
  */
object Task3 {

  val ParsableLine: Regex ="""\s*(\d+)\s+(\d+)\s+(\d+)\s*""".r

  def countPossibleTriangles(data: String): Int =
    parseData(data)
      .count { case (a, b, c) => validateTriangle(a, b, c) }

  private def parseData(data: String): Iterator[(Int, Int, Int)] =
    data
      .lines
      .collect { case ParsableLine(a, b, c) => (a.toInt, b.toInt, c.toInt) }


  def countPossibleTrianglesVertically(data: String): Int =
    parseData(data)
      .map { case (a, b, c) => List(a, b, c) }
      .sliding(3, 3).withPartial(false)
      .flatMap(_.transpose)
      .count { case a :: b :: c :: _ => validateTriangle(a, b, c) }

  def validateTriangle(a: Int, b: Int, c: Int): Boolean = {
    if (a >= b && a >= c) a < b + c
    else validateTriangle(b, c, a)
  }

}
