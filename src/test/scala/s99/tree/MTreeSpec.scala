package s99.tree

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import s99.tree.Tree._

class P70CSpec extends FunSpec with Matchers {
  it("Count the nodes of a multiway tree") {
    MTree('a', List(MTree('f'))).nodeCount should be (2)
    MTree('a', List(MTree('a'), MTree('b'))).nodeCount should be (3)

  }
}

class P70Spec extends FunSpec with Matchers {
  //    a
  //  / | \
  // f  c  b
  // |    / \
  // g   d   e
  val mtree0 = MTree('a', List(
    MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))
  )

  describe("Tree construction from a node string") {
    it("string2MTree") {
      MTree.string2MTree("a") should be (MTree('a', Nil))
      MTree.string2MTree("afg^^c^bd^e^^^") should be (mtree0)
    }
    it("toString") {
      mtree0.toString should be ("afg^^c^bd^e^^")
    }
  }
}
