import Math.{abs => absFrom, exp => expFrom, _}

case class ComplexNumber(real: Double, imaginary: Double) {

  lazy val abs: Double = absFrom(sqrt(real * real + imaginary * imaginary))

  lazy val conjugate: ComplexNumber = ComplexNumber(real, -1 * imaginary)

  def +(that: ComplexNumber): ComplexNumber =
    ComplexNumber(real + that.real, imaginary + that.imaginary)

  def -(that: ComplexNumber): ComplexNumber =
    ComplexNumber(real - that.real, imaginary - that.imaginary)

  def *(that: ComplexNumber): ComplexNumber =
    ComplexNumber(real * that.real - imaginary * that.imaginary,
                  imaginary * that.real + real * that.imaginary)

  def /(that: ComplexNumber): ComplexNumber = {
    val divisor = pow(that.real, 2) + pow(that.imaginary, 2)
    ComplexNumber(
      (real * that.real + imaginary * that.imaginary) / divisor,
      (imaginary * that.real - real * that.imaginary) / divisor
    )
  }

  def *(value: Double): ComplexNumber =
    ComplexNumber(value * real, value * imaginary)
}

object ComplexNumber {
  def apply(real: Int = 0, imaginary: Int = 0): ComplexNumber =
    ComplexNumber(real.toDouble, imaginary.toDouble)

  def exp(complexNumber: ComplexNumber): ComplexNumber =
    ComplexNumber(cos(complexNumber.imaginary), sin(complexNumber.imaginary)) * expFrom(
      complexNumber.real)
}
