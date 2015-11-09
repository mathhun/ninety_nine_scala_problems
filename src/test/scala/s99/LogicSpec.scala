package s99

import org.scalatest._
import s99.Logic._
import s99.Logic.S99Logic._

class P46Spec extends FlatSpec with Matchers {
  "xor" should "" in {
    xor(true, true) should be(false)
    xor(true, false) should be (true)
    xor(false, true) should be (true)
    xor(false, false) should be(false)
  }
}

class P47Spec extends FlatSpec with Matchers {
  "and" should "" in {
    (true and true) should be (true)
    (true and false) should be (false)
    (false and true) should be (false)
    (false and false) should be (false)
  }
}

class P49Spec extends FlatSpec with Matchers {
  "gray" should "generate an n-bit Gray code" in {
    gray(3) should be (List("000", "001", "011", "010", "110", "111", "101", "100"))
  }
}
