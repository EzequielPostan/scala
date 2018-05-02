class DNA(dna: String) {
  // To keep count and checks simple, we first add one nucleotide
  // of each kind. We then avoid counting it.
  lazy val nucleotideCounts: Either[String,Map[Char,Int]] = {
    val modifiedDNA: String = "ATCG" + dna map (_.toUpper)
    val count = modifiedDNA groupBy identity
    if(count.keys.size > 4) Left("Ilegal DNA")
    else Right(count mapValues (_.size - 1))
  }
}
