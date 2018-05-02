object Grains {
  val numberOfSquares: Int = 64
  def square(sq: Int): Option[BigInt] =
    if(sq > numberOfSquares || sq < 1) None
    else Some(BigInt(2).pow(sq-1))

  def total: BigInt = BigInt(2).pow(numberOfSquares) - BigInt(1)
}
