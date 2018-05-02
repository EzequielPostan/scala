object Isogram {
  def isIsogram(s: String): Boolean =
    s.filter(_.isLetter)
     .groupBy(_.toUpper)
     .forall { case (_,v) => v.length == 1 }
}
