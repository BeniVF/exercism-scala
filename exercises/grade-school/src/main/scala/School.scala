import scala.collection.immutable._

class School {
  type DB = Map[Int, Seq[String]]

  private val dbRef = scala.collection.mutable.Map.empty[Int, List[String]]

  def add(name: String, g: Int): Unit = {
    val newValue = dbRef.getOrElseUpdate(g, List.empty[String])
    dbRef += g -> (newValue :+ name)
  }

  def db: DB = dbRef.toMap

  def grade(g: Int): Seq[String] = dbRef.getOrElse(g, List.empty)

  def sorted: DB = TreeMap(dbRef.toSeq.map { case (x, y) => x -> y.sorted }: _*)
}
