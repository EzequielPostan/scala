case class WordCount(phrase: String) {
  def countWords: Map[String, Int] =
    "\\w+('\\w)*".r
      .findAllIn(phrase)
      .toList
      .map(_.toLowerCase)
      .groupBy(identity)
      .mapValues(_.length)
}
