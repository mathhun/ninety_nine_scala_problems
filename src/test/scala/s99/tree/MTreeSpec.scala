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
