package logic

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
}
