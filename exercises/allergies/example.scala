import AllergenExample.AllergenExample

object AllergiesExample {
  private lazy val allergenList = AllergenExample.values

  def allergicTo(allergen: AllergenExample, score: Int): Boolean =
    (allergen.id & score) != 0

  def list(score: Int): List[AllergenExample] =
    allergenList.filter(a => allergicTo(a, score)).toList
}

object AllergenExample extends Enumeration {
  type AllergenExample = Value

  val Eggs = Value(1, "Eggs")
  val Peanuts = Value(2, "Peanuts")
  val Shellfish = Value(4, "Shellfish")
  val Strawberries = Value(8, "Strawberries")
  val Tomatoes = Value(16, "Tomatoes")
  val Chocolate = Value(32, "Chocolate")
  val Pollen = Value(64, "Pollen")
  val Cats = Value(128, "Cats")
}