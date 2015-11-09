package s99

object Logic {
  def and(a: Boolean, b: Boolean): Boolean = a && b

  def or(a: Boolean, b: Boolean): Boolean = a || b

  def nand(a: Boolean, b: Boolean): Boolean = !and(a, b)

  def nor(a: Boolean, b: Boolean): Boolean = !or(a, b)

  def xor(a: Boolean, b: Boolean): Boolean = and(or(a, b), nand(a, b))

  class S99Logic(a: Boolean) {
    def and(b: Boolean): Boolean = (a, b) match {
      case (true, true) => true
      case _ => false
    }
  }

  object S99Logic {
    implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)
  }

  // P49 (**) Gray code.
  // An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
  // n = 1: C(1) = ("0", "1").
  // n = 2: C(2) = ("00", "01", "11", "10").
  // n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
  // Find out the construction rules and write a function to generate Gray codes.
  //
  // scala> gray(3)
  // res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
  // See if you can use memoization to make the function more efficient.
  def gray(n: Int): List[String] = {
    if (n == 0) List("")
    else {
      val lower = gray(n - 1)
      (lower map { "0" + _ }) ::: (lower.reverse map { "1" + _ })
    }
  }
}
