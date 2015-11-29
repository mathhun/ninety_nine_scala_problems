package s99.tree

class MTree[+T](val value: T, val children: List[MTree[T]]) {
  //override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
  override def toString = value.toString + children.map(_.toString + "^").mkString("")

  //def nodeCount: Int = 1 + children.map(_.nodeCount).sum
  def nodeCount: Int = children.foldLeft(1)(_ + _.nodeCount)

  override def equals(o: Any) = o match {
    case that: MTree[T] => value == that.value && children == that.children
    case _ => false
  }

  // def internalPathLength: Int = _internalPathLength(0)
  // private def _internalPathLength(depth: Int = 0): Int = {
  //   depth + children.map(_._internalPathLength(depth + 1)).sum
  // }

  def internalPathLength: Int =
    children.foldLeft(0)((r, c) => r + c.nodeCount + c.internalPathLength)

  // def postorder: List[T] = children match {
  //   case Nil => List(value)
  //   case cs => cs.flatMap(_.postorder) ::: List(value)
  // }
  def postorder: List[T] =
    children.flatMap(_.postorder) ::: List(value)

  def lispyTree: String = {
    if (children.isEmpty) value.toString
    else "(" + value + " " + children.map(_.lispyTree).mkString(" ") + ")"
  }
}

object MTree {
  def apply[T](value: T, children: List[MTree[T]] = Nil) = new MTree(value, children)

  implicit def stringToMTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int = {
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    }
    def splitChildStrings(pos: Int): List[String] =
      if (pos + 1 >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    MTree(s(0), splitChildStrings(1).map(stringToMTree(_)))
  }
}
