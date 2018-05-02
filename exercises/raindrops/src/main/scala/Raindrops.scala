object Raindrops {
  def convert(n: Int): String = {
    val pling: String = if (n % 3 == 0) "Pling" else ""
    val plang: String = if (n % 5 == 0) "Plang" else ""
    val plong: String = if (n % 7 == 0) "Plong" else ""

    if(pling.isEmpty && plang.isEmpty && plong.isEmpty) n.toString
    else List(pling, plang, plong).mkString
  }





}

