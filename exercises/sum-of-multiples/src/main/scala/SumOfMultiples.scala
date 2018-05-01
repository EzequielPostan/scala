object SumOfMultiples {
  
  def sum(factors: Set[Int], limit: Int): Int =
    (for(f <- factors;
         i <- f until limit by f
    ) yield i).sum
}

