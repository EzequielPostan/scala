object Bob {

  def response(statement: String): String = {
    def isAQuestion: Boolean = statement.trim.endsWith("?")
    def isYelling: Boolean = {
      val letters: String = statement.filter(_.isLetter)
      letters.nonEmpty && letters.forall(_.isUpper)
    }
    def saysNothing: Boolean = statement.trim.isEmpty

    if(isAQuestion && isYelling) "Calm down, I know what I'm doing!"
    else if (isAQuestion) "Sure."
    else if (isYelling) "Whoa, chill out!"
    else if (saysNothing) "Fine. Be that way!"
    else "Whatever."
  }
}
