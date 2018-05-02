object RnaTranscription {

  private val conversionTable = Map('G' -> 'C', 'C' -> 'G', 'T' -> 'A', 'A' -> 'U')

  def toRna(dna: String): Option[String] = {
    val initialBuilder = StringBuilder.newBuilder
    val maybeTranslation =
      (dna foldLeft (Some(initialBuilder) : Option[StringBuilder])) { case (acc, c) =>
        for(strand <- acc;
            rna    <- conversionTable.get(c)
        ) yield initialBuilder += rna
      }

    maybeTranslation map (_.mkString)
  }
}
