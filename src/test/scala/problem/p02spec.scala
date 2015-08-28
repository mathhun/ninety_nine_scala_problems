import org.scalatest._

class P02Spec extends FlatSpec with Matchers {
  import P02._

  "penultimate" should "find the last but one element of a list" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
  }
}
