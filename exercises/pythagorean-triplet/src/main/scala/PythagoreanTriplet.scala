object PythagoreanTriplet {

  def sort(triplet: (Int, Int, Int)): (Int, Int, Int) = {
    val (a, b, c) = triplet
    List(a, b, c).sorted match {
      case List(x, y, z) => (x, y , z)
    }
  }


  def pythagoreanTriplets(minFactor: Int, maxFactor:Int): Seq[(Int, Int, Int)] = {
    for( a <- minFactor to maxFactor;
         b <- a to maxFactor;
         if a*a + b*b <= maxFactor*maxFactor;
         c = math.sqrt(a*a + b*b).toInt;
         if c*c == a*a + b*b
    ) yield (a, b, c)
  }

  def isPythagorean(triplet: (Int, Int, Int)): Boolean = {
    val (a, b, c) = sort(triplet)
    a*a + b*b == c*c
  }


  def readmeSolution: Option[(Int, Int, Int)] = explore(1,500,1000).headOption
  def explore(min: Int, max: Int, targetSum: Int): Seq[(Int, Int, Int)] = {
    pythagoreanTriplets(min, max) filter { case (a, b, c) => a + b + c == targetSum}
  }

}
