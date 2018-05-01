object Hexadecimal {

  private def hexDigit(c: Char): Option[Int] =
    if('0' <= c && c <= '9') Some(c.asDigit)
    else if('a' <= c && c <= 'f') Some(10 + c - 'a')
    else if('A' <= c && c <= 'F') Some(10 + c - 'A')
    else None

  def maybeHexToInt(s: String): Option[Int] =
    (s foldLeft (Some(0): Option[Int])) {case (maybeAcc, n) =>
      for(acc  <- maybeAcc;
          next <- hexDigit(n)
      ) yield 16 * acc + next
     }

  def hexToInt(s: String) : Int = maybeHexToInt(s) getOrElse 0
}
