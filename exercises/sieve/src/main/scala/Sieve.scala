object Sieve {
  def primes(upperBound: Int): List[Int] = {
    def removeMultiples(p: Int, ls: List[Int]): List[Int] = ls filter (_ % p != 0)

    @scala.annotation.tailrec
    def primesTailRec(primesList: List[Int], candidates: List[Int]): List[Int] = candidates match {
      case Nil   => primesList.reverse
      case p::ps => primesTailRec(p :: primesList, removeMultiples(p, candidates))
    }
    primesTailRec(Nil, (2 to upperBound).toList)
  }
}
