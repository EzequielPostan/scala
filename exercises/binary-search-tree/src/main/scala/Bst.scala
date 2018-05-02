sealed abstract class Bst[+T] {
  // The first 3 methods below are just here to avoid refactoring the entire set of tests
  // I honestly wouldn't add them and would implement a find method.
  def value: Option[T]
  def left: Option[Bst[T]]
  def right: Option[Bst[T]]
  def insert[U >: T <% Ordered[U]](x: U): Bst[U]
  def toList: List[T]
}

case class  Node[+T](nodeValue: T, leftSubTree: Bst[T], rightSubTree: Bst[T]) extends Bst[T] {
  override def value: Option[T] = Some(nodeValue)
  override def left: Option[Bst[T]] = Some(leftSubTree)
  override def right: Option[Bst[T]] = Some(rightSubTree)
  override def insert[U >: T <% Ordered[U]](x: U): Bst[U] = {
    if(x <= nodeValue) Node(nodeValue, leftSubTree insert x, rightSubTree)
    else Node(nodeValue, leftSubTree, rightSubTree insert x)
  }
  override def toList: List[T] = leftSubTree.toList ++ List(nodeValue) ++ rightSubTree.toList
}
case object Leaf extends Bst[Nothing] {
  override def value: Option[Nothing] = None
  override def left: Option[Bst[Nothing]] = None
  override def right: Option[Bst[Nothing]] = None
  override def insert[U <% Ordered[U]](x: U): Bst[U] = Node(x, Leaf, Leaf)
  override def toList: List[Nothing] = Nil
}

object Bst {
  def apply[T](): Bst[T] = Leaf
  def apply[T](value: T): Bst[T] = new Node(value, Leaf, Leaf)

  def fromList[U <% Ordered[U]](l: List[U]): Bst[U] = (l foldLeft Bst[U]())(_ insert _)
  def toList[T](tree: Bst[T]): List[T] = tree.toList
}