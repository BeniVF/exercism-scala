sealed trait CustomSet[+T]
object CustomSet {
  private final case class NonEmpty[T] private (head: List[T]) extends CustomSet[T]
  private case object Empty extends CustomSet[Nothing]

  private def fold[A, B](set: CustomSet[A])(z: B)(f: List[A] => B): B =
    set match {
      case Empty        => z
      case NonEmpty(xs) => f(xs)
    }

  private def emptySet[T]: CustomSet[T] = Empty

  private def from[T](value: T*): CustomSet[T] =
    CustomSet.fromList(value.toList)

  def fromList[T](xs: List[T]): CustomSet[T] =
    xs match {
      case Nil => Empty
      case _   => NonEmpty(xs.distinct)
    }
  def empty[T](set: CustomSet[T]): Boolean =
    fold(set)(true)(_ => false)

  def member[T](set: CustomSet[T], t: T): Boolean =
    fold(set)(false)(_.contains(t))

  def isSubsetOf[T](first: CustomSet[T], second: CustomSet[T]): Boolean =
    fold(first)(true) { a =>
      fold(second)(false)(b => a.forall(b.contains))
    }

  def isDisjointFrom[T](first: CustomSet[T], second: CustomSet[T]): Boolean =
    fold(first)(true) { a =>
      fold(second)(true)(b => !a.exists(b.contains))
    }

  def isEqual[T](first: CustomSet[T], second: CustomSet[T]): Boolean =
    fold(first)(fold(second)(true)(_ => false)) { a =>
      fold(second)(false) { b =>
        a.size == b.size && a.groupBy(identity).mapValues(_.length) == b
          .groupBy(identity)
          .mapValues(_.length)
      }
    }

  def insert[T](set: CustomSet[T], t: T): CustomSet[T] =
    fold(set)(CustomSet.from(t)) { xs =>
      CustomSet.fromList(t :: xs)
    }

  def intersection[T](first: CustomSet[T], second: CustomSet[T]): CustomSet[T] =
    fold(first)(emptySet[T]) { a =>
      fold(second)(emptySet[T]) { b =>
        CustomSet.fromList(a.intersect(b))
      }
    }

  def difference[T](first: CustomSet[T], second: CustomSet[T]): CustomSet[T] =
    fold(first)(emptySet[T]) { a =>
      fold(second)(first) { b =>
        CustomSet.fromList(a.diff(b))
      }
    }

  def union[T](first: CustomSet[T], second: CustomSet[T]): CustomSet[T] =
    fold(first)(second) { a =>
      fold(second)(first) { b =>
        CustomSet.fromList(a ++ b)
      }
    }

}
