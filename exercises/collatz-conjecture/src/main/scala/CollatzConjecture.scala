object CollatzConjecture {
  def steps(n: Int): Option[Int] = {

    @scala.annotation.tailrec
    def stepsTailRec(acc: Int, c: Int): Option[Int] = {
      if(c == 1) Some(acc)
      else if(c % 2 == 0) stepsTailRec(acc + 1, c / 2)
      else stepsTailRec(acc + 1, 3*c + 1)
    }

    if(n <= 0) None
    else stepsTailRec(0, n)
  }
}
