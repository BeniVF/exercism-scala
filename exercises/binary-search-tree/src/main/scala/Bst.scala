sealed trait Bst[T] {
  def value: T
  def left: Option[Bst[T]]
  def right: Option[Bst[T]]
  def insert(t: T)(implicit ordering: Ordering[T]): Bst[T]
}

object Bst {

  def apply[T](value: T): Bst[T] = BstWrapper(BinaryTree(value))

  def fromList[T](value: List[T])(implicit ordering: Ordering[T]): Bst[T] =
    BstWrapper(BinaryTree.fromList(value))

  def toList[T](bst: Bst[T]): List[T] = bst match {
    case BstWrapper(binaryTree) => BinaryTree.toList(binaryTree)
  }

  private final case class BstWrapper[T](binaryTree: BinaryTree[T]) extends Bst[T] {
    override def value: T = binaryTree match {
      case BinaryTree.Node(_, v, _) => v
      case _                        => sys.error(s"It should not be called this method, because it is an empty node")
    }

    override def left: Option[Bst[T]] = binaryTree match {
      case BinaryTree.Node(left, _, _) => Option(BstWrapper(left))
      case _                           => None
    }

    override def right: Option[Bst[T]] = binaryTree match {
      case BinaryTree.Node(_, _, right) => Option(BstWrapper(right))
      case _                            => None
    }
    def insert(value: T)(implicit ordering: Ordering[T]): Bst[T] =
      BstWrapper(BinaryTree.insert(value)(binaryTree))
  }

  private sealed trait BinaryTree[+T]
  private object BinaryTree {
    case class Node[T](left: BinaryTree[T], value: T, right: BinaryTree[T]) extends BinaryTree[T]
    case object Empty extends BinaryTree[Nothing]

    def empty[T]: BinaryTree[T] = Empty
    def apply[T](value: T): BinaryTree[T] = Node(Empty, value, Empty)

    def insert[T: Ordering](value: T)(binaryTree: BinaryTree[T]): BinaryTree[T] =
      binaryTree match {
        case Empty => Node(Empty, value, Empty)
        case Node(left, previousValue, right) if Ordering[T].gt(value, previousValue) =>
          Node(left, previousValue, insert(value)(right))
        case Node(left, previousValue, right) => Node(insert(value)(left), previousValue, right)
      }

    def fromList[T](values: List[T])(implicit ordering: Ordering[T]): BinaryTree[T] =
      values.foldLeft(empty[T]) {
        case (acc, n) =>
          insert(n)(acc)
      }

    def toList[T](binaryTree: BinaryTree[T]): List[T] = binaryTree match {
      case Empty => List.empty
      case Node(left, value, right) =>
        toList(left) ++ List(value) ++ toList(right)
    }
  }
}
