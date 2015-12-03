package s99.graph

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import s99.graph._

// g b - c
// |  \ /
// h   f
//     |
// d   k

object V {
  // graph-term form
  val g0 = Graph.term(
    List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
    List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h'))
  )

  // adjacency-list form
  val g1 = Graph.adjacent(List(
    ('b', List('c', 'f')), ('c', List('b', 'f')), ('d', Nil),
    ('f', List('b', 'c', 'k')), ('g', List('h')), ('h', List('g')),
    ('k', List('f'))
  ))

  // human-readable form
  // [b-c, f-c, g-h, d, f-b, k-f, h-g]

  // graph-term form
  val g2 = Digraph.term(
    List('r', 's', 't', 'u', 'v'),
    List(('s', 'r'), ('s', 'u'), ('u', 'r'), ('u', 's'), ('v', 'u'))
  )

  // adjacency-list form
  val g3 = Digraph.adjacent(List(
    ('r', Nil), ('s', List('r', 'u')), ('t', Nil),
    ('u', List('r', 's')), ('v', List('u')))
  )

  // human-readable form
  // [s>r, t, u>r, s>u, u>s, v>u]

  // graph-term form
  val g4 = Digraph.termLabel(
    List('k', 'm', 'p', 'q'),
    List(('m', 'q', 7), ('p', 'm', 5), ('p', 'q', 9))
  )

  // adjacency-list form
  val g5 = Digraph.adjacentLabel(List(
    ('k', Nil), ('m', List(('q', 7))), ('p', List(('m', 5), ('q', 9))), ('q', Nil)
  ))

  // human-readable form
  // [p>q/9, m>q/7, k, p>m/5]
}

class P80CSpec extends FunSpec with Matchers {
  import V._

  it("g0 == g1") {
    pending
  }

  it("fromString") {
    pending
    // Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm should be (
    //   List(d, k, h, c, f, g, b),List((h,g,()), (k,f,()), (f,b,()), (g,h,()), (f,c,()), (b,c,()))
    // )
  }
}

class P81CSpec extends FunSpec with Matchers {
  import V._

  it("Path from one node to another one") {
    g4.findPaths('p', 'q') should be (List(List('p', 'q'), List('p', 'm', 'q')))
  }
}

class P82CSpec extends FunSpec with Matchers {
  import V._

  it("Cycle from a given node") {
    g1.findCycles('f') should be (List(List('f', 'c', 'b', 'f'), List('f', 'b', 'c', 'f')))
  }
}
