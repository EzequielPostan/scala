object Sieve {

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter(_ % s.head != 0))

  private val primes = sieve(Stream.from(2))

  def primes(upperBound: Int): List[Int] =
    (primes takeWhile (_ <= upperBound)).toList
}