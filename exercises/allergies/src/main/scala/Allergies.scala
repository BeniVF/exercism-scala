import enumeratum._

object Allergies {
  def allergicTo(allergen: Allergen, personScore: Int): Boolean =
    (personScore & Allergen.allergyScore(allergen)) != 0

  def list(personScore: Int): List[Allergen] =
    Allergen.values.filter(allergicTo(_, personScore)).toList
}

sealed trait Allergen extends EnumEntry

object Allergen extends Enum[Allergen] {

  case object Eggs extends Allergen
  case object Peanuts extends Allergen
  case object Shellfish extends Allergen
  case object Strawberries extends Allergen
  case object Tomatoes extends Allergen
  case object Chocolate extends Allergen
  case object Pollen extends Allergen
  case object Cats extends Allergen

  def allergyScore(allergen: Allergen): Int = allergen match {
    case Eggs         => 1
    case Peanuts      => 2
    case Shellfish    => 4
    case Strawberries => 8
    case Tomatoes     => 16
    case Chocolate    => 32
    case Pollen       => 64
    case Cats         => 128
  }

  override def values = findValues
}

