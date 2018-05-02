import scala.collection.immutable.SortedMap

case class School(db: SortedMap[Int, Seq[String]] = SortedMap()) {
  type DB = SortedMap[Int, Seq[String]]

  def add(name: String, g: Int): School =
    School(db.updated(g, grade(g) :+ name))

  def grade(g: Int): Seq[String] =
    db.getOrElse(g, Seq())

  def sorted: DB = db mapValues (_.sorted)
}
