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

  def leafCount: Int

  def leafList[U >: T]: List[U]

  def internalList: List[T]

  def atLevel[U >: T](n: Int): List[U]

  def nodeCount: Int
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

  def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case _ => left.leafCount + right.leafCount
  }

  def leafList[U >: T]: List[U] = (left, right) match {
    case (End, End) => List(value)
    case _ => left.leafList ++ right.leafList
  }

  def internalList: List[T] = (left, right) match {
    case (End, End) => Nil
    case _ => value :: left.internalList ::: right.internalList
  }

  def atLevel[U >: T](n: Int): List[U] = {
    if (n == 1) List(value)
    else left.atLevel(n - 1) ++ right.atLevel(n - 1)
  }

  def nodeCount: Int = left.nodeCount + right.nodeCount + 1

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
  def isSymmetric: Boolean = true

  def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x)

  def leafCount: Int = 0

  def leafList[U]: List[U] = Nil

  def internalList = Nil

  def atLevel[U](n: Int): List[U] = Nil

  def nodeCount: Int = 0

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

  def symmetricBalancedTrees[T](nodes: Int, value: T): List[Tree[T]] = {
    cBalanced(nodes, value).filter(_.isSymmetric)
  }

  def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
    case h if h < 1 => List(End)
    case 1 => List(Node(value))
    case _ => {
      val fullHeight = hbalTrees(height - 1, value)
      val short = hbalTrees(height - 2, value)
      fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
      fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  }

  def minHbalNodes(height: Int): Int = height match {
    case n if n < 1 => 0
    case 1          => 1
    case n          => minHbalNodes(n - 1) + minHbalNodes(n - 2) + 1
  }

  def maxHbalNodes(height: Int): Int = 2 * height - 1

  def minHbalHeight(nodes: Int): Int =
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1

  def maxHbalHeight(nodes: Int): Int = 
    Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last

  def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] =
    (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList
}
