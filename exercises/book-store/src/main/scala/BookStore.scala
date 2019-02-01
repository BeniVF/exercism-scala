object BookStore {

  def total(books: List[Int]): Int = {
    def calculate(groupedBooks: Map[Int, Int], acc: Int): Int =
      groupedBooks match {
        case _ if groupedBooks.isEmpty => acc
        case _ =>
          (1 to groupedBooks.keySet.size).map { groupSize =>
            calculate(removeBooks(groupedBooks, groupSize),
              priceFor(groupSize) + acc)
          }.min
      }

    if (books.isEmpty) 0
    else
      calculate(books.groupBy(identity).mapValues(_.length), 0)
  }

  private def priceFor(n: Int): Int = n match {
    case _ if n == 2 =>
      (n * 800 * 0.95).toInt
    case _ if n == 3 =>
      (n * 800 * 0.90).toInt
    case _ if n == 4 =>
      (n * 800 * 0.80).toInt
    case _ if n == 5 =>
      (n * 800 * 0.75).toInt
    case _ => 800 * n
  }

  private def removeBooks(xs: Map[Int, Int], n: Int): Map[Int, Int] =
    xs.keySet.take(n).foldLeft(xs) {
      case (acc, k) =>
        if (acc(k) == 1)
          acc.-(k)
        else
          acc + ((k, acc(k) - 1))
    }

}