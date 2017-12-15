import scala.annotation.tailrec
import scala.io.Source

object Day4 {

  @tailrec
  private def iterSecure(words: List[String], seen: Set[String] = Set.empty): Boolean =
    words match {
      case Nil => true
      case word :: _ if seen.contains(word) => false
      case word :: rest => iterSecure(rest, seen + word)
    }

  def isSecure(passphrase: String): Boolean = {
    iterSecure(passphrase.split(" ").toList, Set.empty)
  }

  def countSecure(passphrases: Seq[String]): Int = passphrases.count(isSecure)

  def isReallySecure(passphrase: String): Boolean =
    iterSecure(passphrase.split(" ").toList.map(s => s.sorted))

  def countReallySecure(passphrases: Seq[String]): Int =
    passphrases.count(isReallySecure)

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day4input.txt").getLines().toSeq

    println(countSecure(input))
    println(countReallySecure(input))
  }

}
