case class Clock private (standardMinutes: Int) {
  def +(that: Clock): Clock =
    Clock.fromMinutes(standardMinutes + that.standardMinutes)
  def -(that: Clock): Clock =
    Clock.fromMinutes(standardMinutes - that.standardMinutes)
}

object Clock {
  private val minutesInAnHour = 60
  private val minutesInADay = 24 * minutesInAnHour

  def apply(hours: Int, minutes: Int): Clock =
    Clock.fromMinutes(hours * minutesInAnHour + minutes)

  private def fromMinutes(totalMinutes: Int): Clock = {
    val minutesPassed = totalMinutes % minutesInADay
    Clock(
      if (minutesPassed >= 0)
        minutesPassed
      else
        minutesInADay + minutesPassed
    )
  }
}
