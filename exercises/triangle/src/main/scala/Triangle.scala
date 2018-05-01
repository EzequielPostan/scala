case class Triangle(a: Double, b: Double, c: Double) {
  private lazy val hasPositiveSides = a > 0 && b > 0 && c > 0
  private lazy val checksInequality = a + b >= c  &&  a + c >= b && b + c >= a

  lazy val isValid: Boolean = hasPositiveSides && checksInequality
  lazy val isosceles: Boolean = isValid && (a == b || a == c || b == c)
  lazy val equilateral: Boolean = isValid && ( a == b && b == c)
  lazy val scalene: Boolean = isValid && !isosceles
}
