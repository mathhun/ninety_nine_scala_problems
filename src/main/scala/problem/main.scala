package problem

object Main {
   // P01 (*) Find the last element of a list.
   // Example:
   // scala> last(List(1, 1, 2, 3, 5, 8))
   // res0: Int = 8
  def last[A](list: List[A]): A = {
    list.last
  }

   // P02 (*) Find the last but one element of a list.
   // Example:
   // scala> penultimate(List(1, 1, 2, 3, 5, 8))
   // res0: Int = 5
  def penultimate[A](list: List[A]): A = {
    list.takeRight(2).head
  }

   // P03 (*) Find the Kth element of a list.
   // By convention, the first element in the list is element 0.
   // Example:
   // scala> nth(2, List(1, 1, 2, 3, 5, 8))
   // res0: Int = 2
  def nth[A](n: Int, list: List[A]): A = {
    list(n)
  }

  // P04 (*) Find the number of elements of a list.
  // Example:
  // scala> length(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 6
  def length[A](list: List[A]): Int = {
    list.length
  }

  // P05 (*) Reverse a list.
  // Example:
  // scala> reverse(List(1, 1, 2, 3, 5, 8))
  // res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse[A](list: List[A]): List[A] =
    list.reverse
}
