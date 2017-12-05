import scala.io.Source

object Day4 {

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

  def countReallyValid(passphrases: Seq[String]): Int = passphrases.count(isReallyValid)
}

object RunDay4 extends App {
  println(Day4.countValid(Source.fromResource("day4input.txt").getLines().toSeq))
  println(Day4.countReallyValid(Source.fromResource("day4input.txt").getLines().toSeq))
}
