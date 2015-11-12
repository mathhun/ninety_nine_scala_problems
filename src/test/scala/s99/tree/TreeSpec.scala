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
          Node('c', Node('f', End, Node('g')))), true),
        (Node('a',
          Node('b', Node('d'), End),
          Node('c', End, Node('e'))), true),

        // false
        (Node('a', Node('b'), End), false),
        (Node('a', End, Node('b')), false),

        (End, true)
      )
      forAll (cases) { (tree, expected) =>
        tree.isSymmetric should be (expected)
      }
    }
  }
}

class P57Spec extends FunSpec with Matchers {
  it("should add an element to a binary search tree") {
    val r0 = End.addValue(2)
    val r1 = r0.addValue(3)
    val r2 = r1.addValue(0)

    r0 should be (Node(2))
    r1 should be (Node(2, End, Node(3)))
    r2 should be (Node(2, Node(0), Node(3)))
  }
}

class P58Spec extends FunSpec with Matchers {
  it("Generate-and-test paradigm") {
    Tree.symmetricBalancedTrees(5, "x") should be (
      List(
        Node("x", Node("x", End, Node("x")), Node("x", Node("x"), End)),
        Node("x", Node("x", Node("x"), End), Node("x", End, Node("x")))
      )
    )
  }
}

class P59Spec extends FunSpec with Matchers {
  describe("should construct height-balanced binary trees") {
    it("height 1") {
      Tree.hbalTrees(1, "x") should be (List(
        Node("x")
      ))
    }
    it("height 2") {
      Tree.hbalTrees(2, "x") should be (List(
        Node("x", Node("x"), Node("x")),
        Node("x", Node("x"), End),
        Node("x", End, Node("x"))
      ))
    }
    //Tree.hbalTrees(3, "x") should be (List(
    //  Node("x", Node("x", Node("x"), Node("x")), Node("x", Node("x"), Node("x"))),
    //  Node("x", Node("x", Node("x"), Node("x")), Node("x", Node("x"), End))
    //))
  }
}
