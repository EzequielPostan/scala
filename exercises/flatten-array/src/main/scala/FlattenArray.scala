object FlattenArray {
  def flatten(list: List[Any]): List[Any] = {

    @scala.annotation.tailrec
    def flattenTailRec(acc: List[Any], list: List[Any]): List[Any] = list match {
      case Nil => acc.reverse
      case (x: List[_])::xs => flattenTailRec(acc, x ++ xs)
      case x::xs =>
        val filterNull = if(x == null) acc  else x :: acc
        flattenTailRec(filterNull, xs)
    }

    flattenTailRec(Nil, list)
  }
}
