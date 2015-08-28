/*
P01 (*) Find the last element of a list.
Example:
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
*/

import org.scalatest._

class P01Spec extends FlatSpec with Matchers {
  import P01._

  "last" should "find the last element of a list" in {
    last(List(1, 1, 2, 3, 5, 8)) should be (5)
  }
}
