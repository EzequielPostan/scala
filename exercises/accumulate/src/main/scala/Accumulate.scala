class Accumulate {

  // The example solution provided is not optiomal as it is not tail recursive
  // Direct tail recursive solution would need to append elements at the end
  // of the list, leading to bad performance.

  // I decided to follow an implentation using Builder
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = {
    val builder = List.newBuilder[B]
    for(x <- list) builder += f(x)
    builder.result
  }

  // Curious solution
  // An alternative idea could be to apply Calley representation of lists
  // (sometime called difflists or hughes lists). This leads to represent
  // lists as functions to delay concatenation [1].
  // I didn't use this solution, because I imagine that the one above may be more
  // efficient.
  type CayleyList[C] = List[C] => List[C]
  def embed[C](x: C): CayleyList[C] = (ys: List[C]) => List(x) ++ ys
  def toList[C](cayleyList: CayleyList[C]): List[C] = cayleyList(Nil)

  def accumulateCayley[A, B](f: (A) => B, list : List[A]): List[B] = {
    @scala.annotation.tailrec
    def accumulateTailRec(acc: CayleyList[B], rest: List[A]): List[B] = rest match {
      case Nil   => acc(Nil)
      case x::xs => accumulateTailRec(acc compose embed(f(x)), xs)
    }
    accumulateTailRec(identity, list)
  }

  // [1] First section of http://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids.pdf (or just page 4)
}
