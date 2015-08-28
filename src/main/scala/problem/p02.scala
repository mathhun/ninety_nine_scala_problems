/*
Find the last but one element of a list.
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
*/

object P02 {
  def penultimate[A](list: List[A]): A = {
    list.takeRight(2).head
  }
}
