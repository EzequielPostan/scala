object Pangrams {
  def isPangram(input: String): Boolean =
    input.filter(_.isLetter).map(_.toLower).toSet.size == 26
}

