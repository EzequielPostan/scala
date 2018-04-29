//case class BstExample[+T](value: T, left: Option[BstExample[T]], right: Option[BstExample[T]]) {
//  def insert[U >: T <% Ordered[U]](x: U): BstExample[U] = {
//    def insert(x: U, node: Option[BstExample[U]]): Option[BstExample[U]] =
//      node match {
//        case Some(n) => Option(n.insert(x))
//        case _ => Option(new BstExample(x, None, None))
//      }
//
//    if (x <= value) new BstExample(value, insert(x, left), right)
//    else BstExample(value, left, insert(x, right))
//  }
//}
//
//object BstExample {
//  def fromList[T <% Ordered[T]](l: List[T]): BstExample[T] = l match {
//    case x::xs => xs.foldLeft(BstExample(x, None, None))((r, e) => r.insert(e))
//    case x::Nil => BstExample(x, None, None)
//    case Nil => throw new IllegalArgumentException("Tree must not be empty")
//  }
//
//  def toList[T](tree: BstExample[T]): List[T] = toList(Some(tree))
//
//  private def toList[T](tree: Option[BstExample[T]]): List[T] = tree match {
//    case Some(b) => toList(b.left) ++ List(b.value) ++ toList(b.right)
//    case None => List.empty
//  }
//
//  def apply[T](x: T): BstExample[T] = BstExample(x, None, None)
//}