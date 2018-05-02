object Luhn {

  def filterReverse[A](list: List[A])(p: A => Boolean): List[A] = {
    @scala.annotation.tailrec
    def filterReverseTailRec[A](acc: List[A], l: List[A], p: A => Boolean): List[A] = l match {
      case Nil     => acc
      case x :: xs =>
        if (p(x)) filterReverseTailRec(x :: acc, xs, p)
        else filterReverseTailRec(acc, xs, p)
    }
    filterReverseTailRec(Nil, list, p)
  }

  def valid(numberStr: String): Boolean = {
    val reversedWithoutSpaces = filterReverse(numberStr.toList)( _ != ' ')

    val length = reversedWithoutSpaces.length
    val onlyHasDigits = reversedWithoutSpaces.forall(_.isDigit)

    length > 1 &&
      onlyHasDigits &&
      checkSum(reversedWithoutSpaces map (_.asDigit))
  }

  def checkSum(invertedList: List[Int]): Boolean = {
    def doubleMinusNine(n: Int): Int = {
      val double = 2*n
      if(9 < double) double - 9 else double
    }
    def evenlyDivisible(n: Int, by: Int): Boolean = n % by == 0

    val doubleStep =
      invertedList.zipWithIndex map { case (x,i) =>
        if(i % 2 == 1) doubleMinusNine(x)
        else x
    }

    evenlyDivisible(doubleStep.sum, 10)
  }

}
