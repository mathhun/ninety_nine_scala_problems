package s99.tree

import org.scalatest._
import s99.tree.Tree._

class P55Spec extends FlatSpec with Matchers {
  "cBalanced(1)" should "construct completely balanced binary trees" in {
    Tree.cBalanced(1, "x") should be (List(Node("x")))
  }

  "cBalanced(2)" should "construct completely balanced binary trees" in {
    Tree.cBalanced(2, "x") should be (List(
      Node("x", End, Node("x")), Node("x", Node("x"), End)
    ))
  }

  "cBalanced(3)" should "construct completely balanced binary trees" in {
    Tree.cBalanced(3, "x") should be (List(
      Node("x", Node("x"), Node("x"))
    ))
  }

  "cBalanced(4)" should "construct completely balanced binary trees" in {
    Tree.cBalanced(4, "x") should be (List(
      Node("x", Node("x"), Node("x", End, Node("x"))),
      Node("x", Node("x", End, Node("x")), Node("x")),
      Node("x", Node("x"), Node("x", Node("x"), End)),
      Node("x", Node("x", Node("x"), End), Node("x"))
    ))
  }
}
