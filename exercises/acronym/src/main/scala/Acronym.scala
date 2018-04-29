object Acronym {
  // I agree with the example solution and prefer it.
  // I can leave here another posibility using split method (and avoiding calling head)
  def abbreviate(phrase: String): String = {
    (for {
      word <- phrase.split("\\W+")
      head <- word.headOption
    } yield head.toUpper).mkString
  }
}
