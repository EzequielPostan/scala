object Strain {

  def keep[A](seq: Seq[A], p: A => Boolean): Seq[A] =
    (seq foldRight Seq[A]()) { case (x,acc) =>
        if(p(x)) x +: acc
        else acc
    }

  def discard[A](seq: Seq[A], p: A => Boolean): Seq[A] =
    keep(seq, (x: A) => !p(x))


    // Builder solution
  def keep2[A](seq: Seq[A], p: A => Boolean): Seq[A] = {
    val builder = Seq.newBuilder[A]
    for(x <- seq) if(p(x)) builder += x
    builder.result()
  }
}