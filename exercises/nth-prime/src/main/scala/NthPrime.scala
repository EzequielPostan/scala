object NthPrime {

  // This challenge was implemented in a different way tha Sieve challenge

  private val TOP_LIMIT: Int = 100000000
  private def removeMultiples(p: Int, ls: Stream[Int]): Stream[Int] = ls filter (_ % p != 0)

  def primes(nth: Int): Int = {

    @scala.annotation.tailrec
    def primesTailRec(count: Int, candidates: Stream[Int]): Int = candidates match {
      case _ if count == 1 => candidates.head
      case p #:: ps => primesTailRec(count - 1, removeMultiples(p, ps))
    }

    primesTailRec(nth, Stream.from(2).take(TOP_LIMIT))
  }



  def prime(n: Int): Option[Int] =
    if(n <= 0) None
    else Some(primes(n))
}
