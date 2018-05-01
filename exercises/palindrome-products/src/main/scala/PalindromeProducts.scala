
case class PalindromeProducts(minFactor: Int, maxFactor: Int) {

  lazy val (smallest: Option[(Int, Set[(Int, Int)])],
            largest: Option[(Int, Set[(Int, Int)])]) = {

    val palindromes =
      for (a <- minFactor to maxFactor;
           b <- a to maxFactor;
           if isPalindrome(a * b))
        yield (a, b)

    val mapped = palindromes.groupBy(a => a._1 * a._2)

    if (mapped.isEmpty) (None, None)
    else {
      val (min, max) = (mapped foldLeft (Int.MaxValue, Int.MinValue)) { case ((mi, ma), (k, _)) =>
          if(k < mi) (k,ma)
          else if(k > ma) (mi,k)
          else (mi,ma)
      }
      (Some((min, mapped(min).toSet)), Some((max, mapped(max).toSet)))
    }
  }

  // Much faster that string convesion
  private def isPalindrome(number: Int): Boolean = {

    @scala.annotation.tailrec
    def reverse(acc: Int, original: Int): Int = {
      if(original == 0) acc
      else reverse(10 * acc + original % 10, original / 10)
    }

    number == reverse(0, number)
  }
}

