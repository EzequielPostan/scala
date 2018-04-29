sealed abstract class Bst[+T] {
  // The first 3 methods below are just here to avoid refactoring the entire set of tests
  // I honestly wouldn't add them and would implement a find method.
  def value: T
  def left: Bst[T]
  def right: Bst[T]
  def insert[U >: T <% Ordered[U]](x: U): Bst[U]
  def toList: List[T]
}

case class  Node[+T](value: T, left: Bst[T], right: Bst[T]) extends Bst[T] {
  override def insert[U >: T <% Ordered[U]](x: U): Bst[U] = {
    if(x <= value) Node(value, left insert x, right)
    else Node(value, left, right insert x)
  }
  override def toList: List[T] = left.toList ++ List(value) ++ right.toList
}
case object Leaf extends Bst[Nothing] {
  override def value: Nothing = throw new Exception("Leafs have no value!")
  override def left: Bst[Nothing] = throw new Exception("Leafs have no left subtrees!")
  override def right: Bst[Nothing] = throw new Exception("Leafs have no right subtrees!")
  override def insert[U <% Ordered[U]](x: U): Bst[U] = Node(x, Leaf, Leaf)
  override def toList: List[Nothing] = Nil
}

object Bst {
  def apply[T](): Bst[T] = Leaf
  def apply[T](value: T): Bst[T] = new Node(value, Leaf, Leaf)

  def fromList[U <% Ordered[U]](l: List[U]): Bst[U] = (l foldLeft (Leaf: Bst[U]))(_ insert _)
  def toList[T](tree: Bst[T]): List[T] = tree.toList
}