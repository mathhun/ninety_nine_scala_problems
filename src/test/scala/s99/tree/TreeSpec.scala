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

class P60Spec extends FunSpec with Matchers {
  describe("minHbalNodes") {
    it("should take a height and returns MinN") {
      val cases = Table(
        ("nodes", "expected"),
        (1, 1)
      )
      forAll (cases) { (n, e) =>
        Tree.minHbalNodes(n) should be (e)
      }
    }
  }
  describe("maxHbalHeight") {
    it("should count the maximum height H a height-balanced binary tree with N nodes can have") {
      val cases = Table(
        ("nodes", "expected"),
        (1, 1)
      )
      forAll (cases) { (n, e) =>
        Tree.maxHbalHeight(n) should be (e)
      }
    }
  }
  describe("hbalTreesWithNodes") {
    it("should construct all the height-balanced binary trees with a given nuber of nodes") {
      pending
    }
  }
}

class P61Spec extends FunSpec with Matchers {
  it("should count the leaves of a binary tree") {
    val cases = Table(
      ("Tree", "ExpectedValue"),
      (Node('x', Node('x'), End), 1),
      (Node('x', Node('x'), Node('x')), 2),
      (Node('x', Node('x', Node('x', Node('x'))), Node('x')), 2)
    )

    forAll (cases) { (tree, expected) =>
      tree.leafCount should be (expected)
    }
  }
}

class P61ASpec extends FunSpec with Matchers {
  it("should collect the leaves of a binary tree in a list") {
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList should be (
      List('b', 'd', 'e')
    )
  }
}

class P62Spec extends FunSpec with Matchers {
  it("should collect the internal nodes of a binary tree in a list") {
    val cases = Table(
      ("tree", "expected"),
      (Node('a'), Nil),
      (Node('a', Node('b'), Node('c', Node('d'), Node('e'))), List('a', 'c')),
      (Node('a', Node('b', Node('c'), End), Node('d', Node('e'), End)), List('a', 'b', 'd')),
      (End, Nil)
    )

    forAll (cases) { (tree, expected) =>
      tree.internalList should be (expected)
    }
  }
}

class P62BSpec extends FunSpec with Matchers {
  it("collect the nodes at a given level in a list") {
    val cases = Table(
      ("Tree" , "Level", "expected"),
      (Node('a', Node('b'), Node('c', Node('d'), Node('e'))), 2, List('b', 'c')),
      (Node('a', Node('b'), Node('c', Node('d'), Node('e'))), 3, List('d', 'e'))
    )

    forAll (cases) { (tree, level, expected) =>
      tree.atLevel(level) should be (expected)
    }
  }
}

class P63Spec extends FunSpec with Matchers {
  it("should construct a complete binary tree") {
    val cases = Table(
      ("nodes", "expected"),
      (1, Node(1)),
      (2, Node(1, Node(1), End)),
      (3, Node(1, Node(1), Node(1))),
      (4, Node(1, Node(1, Node(1), End), Node(1))),
      (5, Node(1, Node(1, Node(1), Node(1)), Node(1))),
      (6, Node(1, Node(1, Node(1), Node(1)), Node(1, Node(1), End))),
      (7, Node(1, Node(1, Node(1), Node(1)), Node(1, Node(1), Node(1))))
    )
    forAll (cases) { (n, expected) =>
      Tree.completeBinaryTree(n, 1) should be (expected)
    }
  }
}

class P67Spec extends FunSpec with Matchers {
  describe("A string representation of binary trees") {
    it("toString") {
      val cases = Table(
        ("tree", "expected"),
        (Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))), "a(b(d,e),c(,f(g,)))")
      )
      forAll (cases) { (tree, expected) =>
        tree.toString should be (expected)
      }
    }
    it("fromString") {
      val cases = Table(
        ("string", "expected"),
        ("e", Node('e')),
        ("a(b(d,e),c(,f(g,)))", Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))))
      )
      forAll (cases) { (string, expected) =>
        Tree.fromString(string) should be (expected)
      }
    }
  }
}

class P68Spec extends FunSpec with Matchers {
  describe("Preorder and inorder sequences of binary trees") {
    it("preorder") {
      Tree.string2Tree("a(b(d,e),c(,f(g,)))").preorder should be (List('a', 'b', 'd', 'e', 'c', 'f', 'g'))
    }
    it("inorder") {
      Tree.string2Tree("a(b(d,e),c(,f(g,)))").inorder should be (List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
    }
    it("preInTree") {
      val cases = Table(
        ("pre", "in", "expected"),
        (List('a'), List('a'), "a"),
        (List('a', 'b'), List('b', 'a'), "a(b,)"),
        (List('a', 'b'), List('a', 'b'), "a(,b)")
      )
      forAll (cases) { (p, i, expected) =>
        Tree.preInTree(p, i).toString should be (expected)
      }
    }
  }
}
