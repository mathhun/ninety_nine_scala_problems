package s99.tree

class MTree[+T](value: T, children: List[MTree[T]]) {
  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

  //def nodeCount: Int = 1 + children.map(_.nodeCount).sum
  def nodeCount: Int = children.foldLeft(1)(_ + _.nodeCount)
}

object MTree {
  def apply[T](value: T, children: List[MTree[T]] = Nil) = new MTree(value, children)
}
