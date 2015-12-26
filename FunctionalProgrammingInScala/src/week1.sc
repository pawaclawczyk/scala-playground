object session {
  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double): Boolean =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double, x: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0, x)
  }

  sqrt(2.0)
  sqrt(1e-6)
  sqrt(1e30)

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(4, 12)

  def factorial(x: Int): Int = {
    def factorialIter(x: Int, acc: Int): Int =
      if (0 == x) acc else factorialIter(x - 1, acc * x)

    factorialIter(x, 1)
  }

  factorial(5)
}