import org.scalatest._

class P01Spec extends FlatSpec with Matchers {
  import P01._

  "last" should "find the last element of a list" in {
    last(List(1, 1, 2, 3, 5, 8)) should be (8)
  }
}
