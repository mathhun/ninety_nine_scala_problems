package s99.tree

sealed abstract class Tree[+T] {
  def isSymmetric: Boolean = this match {
    case End => true
    case Node(_, l, r) => l.isMirrorOf(r)
  }

  def isMirrorOf[T](that: Tree[T]): Boolean = (this, that) match {
    case (End, End) => true
    case (End, _) => false
    case (_, End) => false
    case (Node(_, l0, r0), Node(_, l1, r1)) => l0.isMirrorOf(l1) && r0.isMirrorOf(r1)
  }
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }
}
