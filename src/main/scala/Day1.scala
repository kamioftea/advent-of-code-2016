import scala.io.Source

object Day1 {

  def sumDuplicates(digits: Seq[Int]): Int =
    digits
      .zip(digits.tail :+ digits.head)
      .collect { case (a, b) if a == b => a }
      .sum


  def sumMirrors(digits: Seq[Int]): Int = {
    val (first, second) =
      digits.splitAt(digits.length / 2)

    first
      .zip(second)
      .collect { case (a, b) if a == b => a * 2 }
      .sum
  }

  def main(args: Array[String]): Unit = {
    val input: Seq[Int] =
      Source.fromResource("day1input.txt").mkString("").trim.map(_.asDigit)

    println(sumDuplicates(input))
    println(sumMirrors(input))
  }
}
