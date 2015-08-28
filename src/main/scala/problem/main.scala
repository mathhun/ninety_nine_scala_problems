object Main {
  /*
   * P01 (*) Find the last element of a list.
   * Example:
   * scala> last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   */
  def last[A](list: List[A]): A = {
    list.last
  }

  /*
   * P02 (*) Find the last but one element of a list.
   * Example:
   * scala> penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   */
  def penultimate[A](list: List[A]): A = {
    list.takeRight(2).head
  }
}
