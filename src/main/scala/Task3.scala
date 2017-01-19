import scala.util.matching.Regex

/**
  * Created by jeff on 19/01/2017.
  */
object Task3 {

  val ParsableLine: Regex ="""\s*(\d+)\s+(\d+)\s+(\d+)\s*""".r

  def countPossibleTriangles(data: String): Int =
    data.lines
      .collect { case ParsableLine(a, b, c) => (a.toInt, b.toInt, c.toInt) }
      .count { case (a, b, c) => validateTriangle(a, b, c) }

  def validateTriangle(a: Int, b: Int, c: Int): Boolean = {
    if(a >= b && a >= c) a < b + c
    else validateTriangle(b, c, a)
  }

}
