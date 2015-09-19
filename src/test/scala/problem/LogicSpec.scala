package logic

import org.scalatest._
import logic.Logic._
import logic.Logic.S99Logic._

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
