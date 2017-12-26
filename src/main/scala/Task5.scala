import java.security.MessageDigest

object Task5 {

  def md5(s: String): String = {
    MessageDigest.getInstance("MD5")
      .digest(s.getBytes)
      .map(0xFF & _)
      .map {
        "%02x".format(_)
      }
      .mkString
  }

  def findPassword(doorID: String): String =
    Iterator.from(0)
      .map(i => md5(doorID + i))
      .filter(_.startsWith("00000"))
      .map(_.charAt(5))
      .take(8)
      .mkString


  def findUnorderedPassword(doorID: String): String = {
    val Matcher = "00000([0-7])(.).*".r

    def iter(pos: Long, code: String = "_" * 8): String = {
      if(!code.contains('_')) return code

      md5(doorID + pos) match {
        case Matcher(p, c) if code(p.toInt) == '_' =>
          val newCode = code.updated(p.toInt, c.charAt(0))
          println(newCode)
          iter(pos + 1, newCode)
        case _ =>
          iter(pos + 1, code)
      }
    }

    iter(0)
  }

  def main(args: Array[String]): Unit = {
    //println(findPassword("ojvtpuvg"))
    println(findUnorderedPassword("ojvtpuvg"))
  }
}
