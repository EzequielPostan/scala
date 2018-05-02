import org.scalatest.{FunSuite, Matchers}

/** @version created manually **/
class ZipperTest extends FunSuite with Matchers {
  def empty[A]: BinTree[A] = Leaf

  def bt[A](v: A, l: BinTree[A], r: BinTree[A]): BinTree[A] = Node(v, l, r)

  def leaf[A](v: A): BinTree[A] = Node(v, Leaf, Leaf)

  def fromSome[T](o: Option[T]): T = o.get

  val t1: BinTree[Int] = Node(1, bt(2, empty,   leaf(3)), leaf(4))
  val t2: BinTree[Int] = Node(1, bt(5, empty,   leaf(3)), leaf(4))
  val t3: BinTree[Int] = Node(1, bt(2, leaf(5), leaf(3)), leaf(4))
  val t4: BinTree[Int] = Node(1, leaf(2),                 leaf(4))

  val z = Zipper

  test("data is retained") {
   z.toTree(z.fromTree(t1)) should be (t1)
  }


  test("dead end") {
    (z.left(fromSome(z.left(fromSome(z.left(z.fromTree(t1))))))) should be (None)
  }

  test("tree from deep focus") {
    z.toTree(fromSome(z.right(fromSome(z.left(z.fromTree(t1)))))) should be (t1)
  }

  test("setValue") {
    z.toTree(z.setValue(5, (fromSome(z.left(z.fromTree(t1)))))) should be (t2)
  }

  test("setLeft with Some") {
    z.toTree(fromSome(z.setLeft(Node(5, Leaf, Leaf),
        (fromSome(z.left(z.fromTree(t1))))))) should be (t3)
  }

  test("setRight with None") {
    z.toTree(fromSome(z.setRight(Leaf, (fromSome(z.left(z.fromTree(t1))))))) should be (t4)
  }

  test("different paths to same zipper") {
    z.right(fromSome(z.up(fromSome(z.left(z.fromTree(t1)))))) should be
      (z.right(z.fromTree(t1)))
  }
}

