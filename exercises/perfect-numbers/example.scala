import NumberTypeExample.NumberTypeExample

object PerfectNumbersExample {
  def classify(n: Int): Either[String, NumberTypeExample] = {
    if (n <= 0)
      Left("Classification is only possible for natural numbers.")
    else {
      val sumOfFactors
      = (1 until n)
        .foldLeft(0)((acc, i) => if (n % i == 0) acc + i else acc)

      if (sumOfFactors < n)
        Right(NumberTypeExample.Deficient)
      else if (sumOfFactors > n)
        Right(NumberTypeExample.Abundant)
      else
        Right(NumberTypeExample.Perfect)
    }
  }
}

object NumberTypeExample extends Enumeration {
  type NumberTypeExample = Value

  val Perfect = Value("Perfect")
  val Abundant = Value("Abundant")
  val Deficient = Value("Deficient")
}