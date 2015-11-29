package s99.tree

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import s99.tree.MTree._

object V {
  //    a
  //  / | \
  // f  c  b
  // |    / \
  // g   d   e
  val mtree0 = MTree('a', List(
    MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))
  )
}
import V._

class P70CSpec extends FunSpec with Matchers {
  it("Count the nodes of a multiway tree") {
    MTree('a', List(MTree('f'))).nodeCount should be (2)
    MTree('a', List(MTree('a'), MTree('b'))).nodeCount should be (3)
  }
}

class P70Spec extends FunSpec with Matchers {
  describe("Tree construction from a node string") {
    it("string2MTree") {
      MTree.stringToMTree("a") should be (MTree('a', Nil))
      MTree.stringToMTree("afg^^c^bd^e^^^") should be (mtree0)
    }
    it("implicit conversion") {
      "afg^^c^bd^e^^^".value should be ('a')
      "afg^^c^bd^e^^^".children should be (List(
        MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))
      ))
    }
    it("toString") {
      mtree0.toString should be ("afg^^c^bd^e^^")
    }
  }
}

class P71Spec extends FunSpec with Matchers {
  val mtree0 = MTree('a', List(
    MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))
  )

  it("should determine the internal path length of a tree") {
    "afg^^c^bd^e^^^".internalPathLength should be (9)
    mtree0.internalPathLength should be (9)
  }
}
