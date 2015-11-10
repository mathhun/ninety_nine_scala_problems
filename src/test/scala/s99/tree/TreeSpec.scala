package s99.tree

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
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
    it("should be true") {
      val cases = Table(
        ("Tree", "ExpectedValue"),
        // true
        (Node('a', Node('b'), Node('c')), true),
        (Node('a',
          Node('b', Node('d', Node('e'), End)),
          Node('c', Node('f', Node('g'), End))), true),

        // false
        (Node('a', Node('b'), End), false),
        (Node('a', End, Node('b')), false),
        (Node('a',
          Node('b', Node('d'), End),
          Node('c', End, Node('e'))), false)
      )
      forAll (cases) { (tree, expected) =>
        tree.isSymmetric should be (expected)
      }
    }
  }
}
