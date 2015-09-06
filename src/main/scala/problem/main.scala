package problem

object Main {
   // P01 (*) Find the last element of a list.
   // Example:
   // scala> last(List(1, 1, 2, 3, 5, 8))
   // res0: Int = 8
  def last[A](list: List[A]): A =
    list.last

   // P02 (*) Find the last but one element of a list.
   // Example:
   // scala> penultimate(List(1, 1, 2, 3, 5, 8))
   // res0: Int = 5
  def penultimate[A](list: List[A]): A =
    list.takeRight(2).head

   // P03 (*) Find the Kth element of a list.
   // By convention, the first element in the list is element 0.
   // Example:
   // scala> nth(2, List(1, 1, 2, 3, 5, 8))
   // res0: Int = 2
  def nth[A](n: Int, list: List[A]): A =
    list(n)

  // P04 (*) Find the number of elements of a list.
  // Example:
  // scala> length(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 6
  def length[A](list: List[A]): Int =
    list.length

  // P05 (*) Reverse a list.
  // Example:
  // scala> reverse(List(1, 1, 2, 3, 5, 8))
  // res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse[A](list: List[A]): List[A] =
    list.reverse

  // P06 (*) Find out whether a list is a palindrome.
  // Example:
  // scala> isPalindrome(List(1, 2, 3, 2, 1))
  // res0: Boolean = true
  def isPalindrome[A](list: List[A]): Boolean =
    list == list.reverse

  // P07 (**) Flatten a nested list structure.
  // Example:
  // scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  // res0: List[Any] = List(1, 1, 2, 3, 5, 8)

  //def flatten(list: List[Any]): List[Any] = {
  //  if (list.isEmpty) Nil
  //  else list.head match {
  //    case x :: xs => x :: flatten(xs) ++ flatten(list.tail)
  //    case x => x +: flatten(list.tail)
  //  }
  //}
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  // If a list contains repeated elements they should be replaced with
  // a single copy of the element. The order of the elements should
  // not be changed.
  // Example:
  // scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  def compress[A](list: List[A]): List[A] = {
    def go[A](ls: List[A], acc: List[A]): List[A] = {
      if (ls.isEmpty) acc.reverse
      else if (acc.isEmpty) go(ls.tail, List(ls.head))
      else if (ls.head == acc.head) go(ls.tail, acc)
      else go(ls.tail, ls.head :: acc)
    }
    go(list, Nil)
  }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  // If a list contains repeated elements they should be placed in separate sublists.
  // Example:
  // scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack[A](list: List[A]): List[List[A]] = {
    def go[A](ls: List[A], as: List[A], ass: List[List[A]]): List[List[A]] = {
      if (ls.isEmpty) (as :: ass).reverse
      else if (as.isEmpty) go(ls.tail, List(ls.head), ass)
      else if (ls.head == as.head) go(ls.tail, ls.head :: as, ass)
      else go(ls.tail, List(ls.head), as :: ass)
    }
    go(list, Nil, Nil)
  }

  def pack_answer[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  // P10 (*) Run-length encoding of a list.
  // Use the result of problem P09 to implement the so-called
  // run-length encoding data compression method. Consecutive
  // duplicates of elements are encoded as tuples (N, E) where N is
  // the number of duplicates of the element E.
  // Example:
  // scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode[A](ls: List[A]): List[(Int, A)] = {
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List((packed.length, packed.head))
      else (packed.length, packed.head) :: encode(next)
    }
  }

  def encode_answer[A](ls: List[A]): List[(Int, A)] =
    pack(ls) map { e => (e.length, e.head) }

  // P11 (*) Modified run-length encoding.
  // Modify the result of problem P10 in such a way that if an element
  // has no duplicates it is simply copied into the result list. Only
  // elements with duplicates are transferred as (N, E) terms.
  // Example:
  // scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  def encodeModified[A](ls: List[A]): List[Any] = {
    pack(ls) map { e =>
      if (e.length == 1) e.head
      else (e.length, e.head)
    }
  }

  // P12 (**) Decode a run-length encoded list.
  // Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  // Example:
  // scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  // res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  def decode[A](ls: List[(Int, A)]): List[A] = {
    ls.foldRight(Nil: List[A])((x, acc) => List.fill(x._1)(x._2) ++ acc)
  }

  def decode_answer[A](ls: List[(Int, A)]): List[A] = {
    ls flatMap { e => List.fill(e._1)(e._2) }
  }

  // P13 (**) Run-length encoding of a list (direct solution).
  // Implement the so-called run-length encoding data compression
  // method directly. I.e. don't use other methods you've written
  // (like P09's pack); do all the work directly.
  // Example:
  // scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    if (ls.isEmpty) List()
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List((packed.length, packed.head))
      else (packed.length, packed.head) :: encodeDirect(next)
    }
  }

  // P14 (*) Duplicate the elements of a list.
  // Example:
  // scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  def duplicate[A](ls: List[A]): List[A] = {
    ls flatMap { x => List(x, x) }
  }

  // P15 (**) Duplicate the elements of a list a given number of times.
  // Example:
  // scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  def duplicateN[A](n: Int, ls: List[A]): List[A] = {
    ls flatMap { x => List.fill(n)(x) }
  }

  // P16 (**) Drop every Nth element from a list.
  // Example:
  // scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  def drop[A](n: Int, ls: List[A]): List[A] = {
    if (ls.isEmpty) List()
    else {
      val hd = ls.take(n)
      if (n == hd.length) {
        hd.dropRight(1) ++ Main.drop(n, ls.drop(n))
      } else {
        hd ++ Main.drop(n, ls.drop(n))
      }
    }
  }

  def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
      case (_, Nil) => Nil
      case (1, _ :: tail) => dropR(n, tail)
      case (_, h :: tail) => h :: dropR(c - 1, tail)
    }
    dropR(n, ls)
  }

  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil) => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }

  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  // P17 (*) Split a list into two parts.
  // The length of the first part is given. Use a Tuple for your result.
  // Example:
  // scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    (ls.take(n), ls.drop(n))
  }

  // P18 (**) Extract a slice from a list.
  // Given two indices, I and K, the slice is the list containing the
  // elements from and including the Ith element up to but not
  // including the Kth element of the original list. Start counting
  // the elements with 0.  Example:
  // scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g)
  def slice[A](s: Int, e: Int, ls: List[A]): List[A] = {
    ls.take(e).drop(s)
  }

  def sliceBuiltin[A](s: Int, e: Int, ls: List[A]): List[A] = {
    ls.slice(s, e)
  }

  // P19 (**) Rotate a list N places to the left.
  // Examples:
  // scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  // scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    if (n > 0) ls.drop(n) ++ ls.take(n)
    else ls.takeRight(-n) ++ ls.dropRight(-n)
  }
}
