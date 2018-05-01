object Change {
  def findFewestCoins(target: Int, coinList: List[Int]): Option[List[Int]] = {
    def findFewestCoinsRec(bestResult: Option[List[Int]],
                           current: List[Int],
                           amount: Int,
                           coins: List[Int]): Option[List[Int]] = {

      def currentIsWorse: Boolean = bestResult map (_.length < current.length) getOrElse false

      if(amount < 0 || currentIsWorse) bestResult
      else if(amount == 0) Some(current)
      else {
        coins match {
          case c::cs if c <= amount =>
            val addingC = findFewestCoinsRec(bestResult, c :: current, amount - c, coins)
            findFewestCoinsRec(addingC, current, amount, cs)
          case c::cs if c > amount =>
            findFewestCoinsRec(bestResult, current, amount, cs)
          case _   => bestResult
        }
      }
    }

    findFewestCoinsRec(None, Nil, target, coinList.sorted(Ordering.Int.reverse))
  }
}

