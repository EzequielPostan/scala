object Series {
  def slices(n: Int, s: String): Seq[Seq[Int]] =
    for(tail <- s.tails.toSeq;
        slice = tail take n;
        if slice.length == n
    ) yield slice map (_.asDigit)
}
