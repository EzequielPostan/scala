object PascalsTriangle {

  private def addWithItsTail(xs: List[Int]): List[Int] = xs match {
    case Nil   => Nil
    case x::xs => (x::xs) zip xs map { case (a: Int, b: Int) => a + b }
  }

  def rows(number: Int): List[List[Int]] = {

    @scala.annotation.tailrec
    def rowsTailRec(acc: List[List[Int]], n: Int): List[List[Int]] = {
      if (n <= 0) acc
      else {
        val firstRows: List[List[Int]] = acc
        val nextRow: List[Int] =
          firstRows.lastOption match {
            case None     => List(1)
            case Some(xs) =>
              List(1) ++ addWithItsTail(xs) ++ List(1)
          }
        rowsTailRec(firstRows ++ List(nextRow), n - 1)
      }
    }

    rowsTailRec(Nil, number)
  }


  // Non tail recursive form
  def rowsNonTailRec(n: Int): List[List[Int]] =
    if(n <= 0) List()
    else {
      val firstRows: List[List[Int]] = rows(n-1)
      val nextRow: List[Int] =
        firstRows.lastOption match {
          case None     => List(1)
          case Some(xs) =>
            List(1) ++ addWithItsTail(xs) ++ List(1)
        }

      firstRows ++ List(nextRow)
    }

}
