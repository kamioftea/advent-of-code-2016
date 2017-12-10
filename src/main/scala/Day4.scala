import scala.annotation.tailrec
import scala.io.Source

object Day4 {

  @tailrec
  def isValid(words: List[String], seen: Set[String] = Set.empty): Boolean =
    words match {
      case Nil => true
      case word :: _ if seen.contains(word) => false
      case word :: rest => isValid(rest, seen + word)
    }

  def isValid(passphrase: String): Boolean = {
    isValid(passphrase.split(" ").toList, Set.empty)
  }

  def countValid(passphrases: Seq[String]): Int = passphrases.count(isValid)

  def isReallyValid(passphrase: String): Boolean =
    isValid(passphrase.split(" ").toList.map(s => s.sorted))

  def countReallyValid(passphrases: Seq[String]): Int =
    passphrases.count(isReallyValid)

  def main(args: Array[String]): Unit = {
    println(
      countValid(Source.fromResource("day4input.txt").getLines().toSeq)
    )

    println(
      countReallyValid(Source.fromResource("day4input.txt").getLines().toSeq)
    )
  }

}
