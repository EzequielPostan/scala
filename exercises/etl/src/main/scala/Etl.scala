object Etl {
  def transform(m: Map[Int, Seq[String]]): Map[String, Int] =
    for { (points, letters) <- m
          c <- letters
    } yield (c map (_.toLower)) -> points
}
