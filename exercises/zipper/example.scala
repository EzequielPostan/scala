object ZipperExample {
  // implementation translated from http://learnyouahaskell.com/zippers
  type Breadcrumbs[A] = List[Crumb[A]]
  // A zipper for a binary tree.
  type Zipper[A] = (FakeBinTree[A], Breadcrumbs[A])

  // Get a zipper focussed on the root node.
  def fromTree[A](bt: FakeBinTree[A]): Zipper[A] = (bt, Nil)

  // Get the complete tree from a zipper.
  def toTree[A](zipper: Zipper[A]): FakeBinTree[A] = zipper match {
    case (bt, Nil) => bt
    case _ => toTree(up(zipper).get)
  }

  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): A = zipper._1.value

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (FakeBinTree(x, Some(l), r), bs) => Some((l, LeftCrumb(x, r)::bs))
    case _ => None
  }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (FakeBinTree(x, l, Some(r)), bs) => Some((r, RightCrumb(x, l)::bs))
    case _ => None
  }

  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper match {
    case (t, LeftCrumb(x, r)::bs) => Some((FakeBinTree(x, Some(t), r), bs))
    case (t, RightCrumb(x, l)::bs) => Some((FakeBinTree(x, l, Some(t)), bs))
    case _ => throw new Exception("up: called on topmost focus")
  }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = {
    val (bt, bs) = zipper
    (bt.copy(value = v), bs)
  }

  // Replace a left child tree.
  def setLeft[A](l: Option[FakeBinTree[A]], zipper: Zipper[A]): Zipper[A] = {
    val (bt, bs) = zipper
    (bt.copy(left = l), bs)
  }

  // Replace a right child tree.
  def setRight[A](r: Option[FakeBinTree[A]], zipper: Zipper[A]): Zipper[A] = {
    val (bt, bs) = zipper
    (bt.copy(right = r), bs)
  }
}

// A binary tree.
case class FakeBinTree[A](value: A, left: Option[FakeBinTree[A]], right: Option[FakeBinTree[A]])

trait Crumb[A]
case class LeftCrumb[A](value: A, tree: Option[FakeBinTree[A]]) extends Crumb[A]
case class RightCrumb[A](value: A, tree: Option[FakeBinTree[A]]) extends Crumb[A]
