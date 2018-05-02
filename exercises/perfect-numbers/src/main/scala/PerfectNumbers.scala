object PerfectNumbers {
  import NumberType.{NumType, Abundant, Deficient, Perfect}

  val ErrorMessage: String = "Classification is only possible for natural numbers."

  def classify(n: Int): Either[String, NumType] = {
    //This auxiliary method could trivially be optimised using a tail recursive function
    def sumOfFactors: Int = ((1 until n) filter (n % _ == 0)).sum

    if(n <= 0) Left(ErrorMessage)
    else if (n == sumOfFactors) Right(Perfect)
    else if (n < sumOfFactors) Right(Abundant)
    else Right(Deficient)
  }
}

object NumberType {
  sealed abstract class NumType
  case object Perfect extends NumType
  case object Abundant extends NumType
  case object Deficient extends NumType
}
