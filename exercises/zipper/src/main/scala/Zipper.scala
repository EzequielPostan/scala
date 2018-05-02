object Zipper {

  // Implementation also based on http://learnyouahaskell.com/zippers
  // My implementation keeps the idea of Leafs (empty trees
  // Tests were modified, the ideas are the same

  // A zipper for a binary tree.
  type Zipper[A] = (BinTree[A], List[SubTree[A]])

  // Get a zipper focussed on the root node.
  def fromTree[A](bt: BinTree[A]): Zipper[A] = (bt, Nil)

  // Get the complete tree from a zipper.
  def toTree[A](zipper: Zipper[A]): BinTree[A] = zipper match {
    case (bt, Nil) => bt
    case _ => toTree(up(zipper).get)
  }
  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): Option[A] = zipper._1 match {
    case Leaf        => None
    case Node(v,_,_) => Some(v)
  }

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (Node(x, l, r), ts) => Some(l, LeftSubTree(x,r) :: ts)
    case _                   => None
  }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (Node(x, l, r), ts) => Some(r, RightSubTree(x, l) :: ts)
    case _                   => None
  }
  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (l, LeftSubTree(x, r)::bs)  => Some((Node(x, l, r), bs))
    case (r, RightSubTree(x, l)::bs) => Some((Node(x, l, r), bs))
    case (t, Nil) => None
  }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = zipper match {
    case (Leaf, ts) => (Node(v, Leaf, Leaf), ts)
    case (Node(_,l,r), ts) => (Node(v,l,r), ts)
  }

  // Replace a left child tree.
  def setLeft[A](l: BinTree[A], zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (Leaf,_)  => None
    case (Node(v,_,r),ts) => Some((Node(v,l,r),ts))
  }

  // Replace a right child tree.
  def setRight[A](r: BinTree[A], zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (Leaf,_)  => None
    case (Node(v,l,_),ts) => Some((Node(v,l,r),ts))
  }
}

// A (modified) binary tree.
sealed abstract class BinTree[+A]
case class Node[A](value: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]
case object Leaf extends BinTree[Nothing]

sealed abstract class SubTree[A]
case class RightSubTree[A](value: A, subtree: BinTree[A]) extends SubTree[A]
case class LeftSubTree[A](value: A, subtree: BinTree[A]) extends SubTree[A]

