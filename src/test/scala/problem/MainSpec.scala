import org.scalatest._
import problem.Main._

class P01Spec extends FlatSpec with Matchers {
  "last" should "find the last element of a list" in {
    last(List(1, 1, 2, 3, 5, 8)) should be (8)
  }
}

class P02Spec extends FlatSpec with Matchers {
  "penultimate" should "find the last but one element of a list" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
  }
}

class P03Spec extends FlatSpec with Matchers {
  "nth" should "find the Kth element of a list" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
  }
}

class P04Spec extends FlatSpec with Matchers {
  "length" should "find the number of elements of a list" in {
    problem.Main.length(List(1, 1, 2, 3, 5, 8)) should be (6)
  }
}

class P05Spec extends FlatSpec with Matchers {
  "reverse" should "reverse a list" in {
    reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }
}

class P06Spec extends FlatSpec with Matchers {
  "isPalindrome" should "find out whether a list is a palindrome" in {
    isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
    isPalindrome(List(1, 2, 3, 4, 5)) should be (false)
  }
}
