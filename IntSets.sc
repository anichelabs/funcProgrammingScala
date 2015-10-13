val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4

val t3 = t2 incl 2
t3 union new NonEmpty(5, Empty, Empty)


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if ( x > elem) new NonEmpty(elem, left, right incl x)
    else this

  }
  def contains(x: Int): Boolean = {
    if ( x < elem) left contains x
    else if ( x > elem) right contains x
    else true
  }

  def union(other: IntSet): IntSet = {
    ((other union (left union right) incl elem))
  }

  override def toString = "{" + left + elem +  right + "}"
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet): IntSet = other

  override def toString = "."
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}