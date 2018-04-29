import Allergen._

// I just removed Enumeration and used case objects instead.
// Given the simplicity of the challenge, it makes no major difference.
// However, it is a good place to mention that Enumeration is marked as
// a bad practice. E.g.: Enumeration brings problems with type erasure
// and provides no compiler exhaustive checks on pattern matching.
// Interesting article about the topic: https://underscore.io/blog/posts/2014/09/03/enumerations.html

object Allergies {

  def allergicTo(allergen: AlergyItem, score: Int): Boolean =
    (allergen.score & score) != 0

  def list(score: Int): List[AlergyItem] =
    allListedAllergies filter (a => allergicTo(a, score))
}

object Allergen {

  lazy val allListedAllergies: List[AlergyItem] = List(
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats
  )

  sealed abstract class AlergyItem(val score: Int)
  case object Eggs extends AlergyItem(1)
  case object Peanuts extends AlergyItem(2)
  case object Shellfish extends AlergyItem(4)
  case object Strawberries extends AlergyItem(8)
  case object Tomatoes extends AlergyItem(16)
  case object Chocolate extends AlergyItem(32)
  case object Pollen extends AlergyItem(64)
  case object Cats extends AlergyItem(128)

}
