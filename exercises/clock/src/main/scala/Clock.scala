
/* Interesting section on polymorphic equality in [1].
 * More material in [2]
 *
 * [1] Section 2.5.2 of Scala in Depth (page 25 of https://www.comcol.nl/code/inkijkexemplaar/9781935182702/scala-in-depth-engels-josh-suereth.pdf)
 * [2] See Programming in Scala,Chapter 28 for discussion and design.
 */
case class Clock(minutes: Int) {
  private lazy val normalized = {
    val tmp = minutes % (24 * 60)
    if (tmp >= 0) tmp else (24 * 60) + tmp
  }

  def +(that: Clock): Clock = Clock(normalized + that.normalized)

  def -(that: Clock): Clock = Clock(normalized - that.normalized)

  override def canEqual(other: Any): Boolean = other match {
    case that: Clock => true
    case _ => false
  }

  override def equals(other: Any): Boolean = other match {
    case that: Clock =>
      (this eq that) ||
       ((that.## == this.##) &&
        (that canEqual this) &&
        (normalized == that.normalized ))
    case _ => false
  }

  override def hashCode(): Int = normalized.hashCode()
}

object Clock {
  def apply(hour: Int, min: Int) = new Clock(hour * 60 + min)
}
