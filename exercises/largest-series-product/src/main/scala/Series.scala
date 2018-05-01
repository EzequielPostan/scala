object Series {
  def largestProduct(n: Int, s: String): Option[Int] = {

    @scala.annotation.tailrec
    def largestTailRec(acc: Int, remainingDigits: List[Int]): Int =
      if(remainingDigits.length < n) acc
      else {
        val prod = (remainingDigits take n).product
        largestTailRec(math.max(prod,acc), remainingDigits drop 1)
      }

    val digitsList = s.filter(_.isDigit).map(_.asDigit).toList

    if(n == 0) Some(1)
    else if (n < 0 || digitsList.length < n || s.exists(! _.isDigit)) None
    else Some(largestTailRec(0, digitsList))
  }
}
