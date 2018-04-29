object Anagram {
  // This is similar to the example solution but not calling sorting methods.
  // Note: it is not necesary to count the repetitions with the call to  mapValues (_.length).
  //       It was made just for clarity in this case.
  def anagrams(word: String, anagrams: Seq[String]): Seq[String] = {
    def charCount(w: String): Map[Char, Int] = w.toLowerCase groupBy identity mapValues (_.length)
    def notTheSame(a: String, b: String): Boolean = a.toLowerCase != b.toLowerCase
    val initialWordCharCount = charCount(word)
    anagrams filter (a => initialWordCharCount == charCount(a) && notTheSame(word, a))
  }
}
