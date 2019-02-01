object ArmstrongNumbers {

  def isArmstrongNumber(number: Int): Boolean =
    number.toString
      .map(x => Math.pow(x.toString.toInt, number.toString.length))
      .sum == number

}
