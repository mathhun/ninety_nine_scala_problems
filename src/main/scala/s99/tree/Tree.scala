package s99.tree

sealed abstract class Tree[+T] {
  //def isSymmetric: Boolean = this match {
  //  case End => true
  //  case Node(_, l, r) => l.isMirrorOf(r)
  //}
  //
  //def isMirrorOf[T](that: Tree[T]): Boolean = (this, that) match {
  //  case (End, End) => true
  //  case (End, _) => false
  //  case (_, End) => false
  //  case (Node(_, l0, r0), Node(_, l1, r1)) => l0.isMirrorOf(l1) && r0.isMirrorOf(r1)
  //}
  def isMirrorOf[V](tree: Tree[V]): Boolean
  def isSymmetric: Boolean

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
    case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case _ => false
  }
  def isSymmetric: Boolean = left.isMirrorOf(right)

  //def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = this match {
  //  case Node(e, l, r) if (x < e) => Node(e, l.addValue(x), r)
  //  case Node(e, l, r) => Node(e, l, r.addValue(x))
  //}
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
  def isSymmetric: Boolean = true

  def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x)

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
