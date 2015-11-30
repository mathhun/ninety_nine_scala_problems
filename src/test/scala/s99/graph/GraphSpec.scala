package s99.tree

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import s99.graph

// g b - c
// |  \ /
// h   f
//     |
// d   k

class P80CSpec extends FunSpec with Matchers {
  it("fromString") {
    Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm should be (
      List(d, k, h, c, f, g, b),List((h,g,()), (k,f,()), (f,b,()), (g,h,()), (f,c,()), (b,c,()))
    )
  }
}
