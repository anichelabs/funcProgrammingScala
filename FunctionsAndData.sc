val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x.numer
x.denom


val xPy = x.add(y)

val negX = x.neg

val xSy = x.sub(y)

val xyz = x.sub(y).sub(z)

val added = y.add(y)

x.less(y)

x.max(y)

y.max(x)

class Rational(x: Int, y: Int) {
  require (y > 0, "Denominator must be positive")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b:Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  def add(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom)

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (less(that)) that else this

  override def toString = numer +  "/" + denom
}