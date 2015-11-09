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

class P56Spec extends FunSpec with Matchers {
  describe("isSymmetric") {
    describe("when empty") {
      it("should be true") {
        Node().isSymmetric should be (true)
      }
    }
    describe("when trees are symmetric") {
      it("should be true") {
        Node('a', Node('b'), Node('c')).isSymmetric should be (true)
      }
    }
    describe("when trees are not symmetric") {
      it("should be false") {
        Node('a', Node('b'), End).isSymmetric should be (false)
      }
    }
  }
}
