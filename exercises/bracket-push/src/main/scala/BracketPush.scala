import scala.annotation.tailrec

object BracketPush {

  def isSymbol(c: Char): Boolean = "[]{}()" contains c
  def isOpenSymbol(c: Char): Boolean = "{[(" contains c
  def openingSymbol(c: Char): Char =
    if(c == ')') '('
    else if (c == ']') '['
    else '{'

  def isPaired(s: String): Boolean = {
    val onlySymbols: String = s filter isSymbol

    @tailrec
    def isPairedTailRec(rest: List[Char], stack: List[Char]): Boolean = (rest, stack) match {
      case (Nil  , _)   => stack.isEmpty
      case (x::xs, _) if isOpenSymbol(x) => isPairedTailRec(xs, x :: stack)
      case (x::xs, s::ss) => openingSymbol(x) == s && isPairedTailRec(xs,ss)
      case (_ , Nil)      => false
    }

    isPairedTailRec(onlySymbols.toList, Nil)
  }

}
