object AtbashCipher {
  // Just an alternative simple solution

  private val charMap: Map[Char, Char] =
    (for(c <- 'a' to 'z') yield (c, ('a' + 'z' - c).toChar)).toMap

  def encodeChar(c: Char): Option[Char] =
    if(c.isDigit) Some(c)
    else if(c.isLetter) charMap.get(c)
    else None

  def encode(s: String): String =
    (s.toLowerCase flatMap encodeChar grouped 5) mkString " "

  def decode(s: String): String =
    s.toLowerCase flatMap encodeChar

}
