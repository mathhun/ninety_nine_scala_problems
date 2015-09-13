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

  // P20 (*) Remove the Kth element from a list.
  // Return the list and the removed element in a Tuple. Elements are numbered from 0.
  // Example:
  // scala> removeAt(1, List('a, 'b, 'c, 'd))
  // res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    (ls.take(n) ++ ls.drop(n + 1), ls(n))
  }

  def removeAt_answer[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }

  // P21 (*) Insert an element at a given position into a list.
  // Example:
  // scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  // res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt[A](a: A, n: Int, ls: List[A]): List[A] = {
    val x = ls.splitAt(n)
    x._1 ++ (a :: x._2)
  }

  def insertAt_answer[A](e: A, n: Int, ls: List[A]): List[A] = ls.splitAt(n) match {
    case (pre, post) => pre ::: e :: post
  }

  // P22 (*) Create a list containing all integers within a given range.
  // Example:
  // scala> range(4, 9)
  // res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  def range(s: Int, e: Int): List[Int] = {
    if (s > e) Nil
    else s :: range(s + 1, e)
  }

  // P23 (**) Extract a given number of randomly selected elements from a list.
  // Example:
  // scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  // res0: List[Symbol] = List('e, 'd, 'a)
  // Hint: Use the solution to problem P20
  import scala.util.Random
  import scala.collection.mutable.ListBuffer
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }

  // P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  // Example:
  // scala> lotto(6, 49)
  // res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  def lotto(n: Int, max: Int): List[Int] = {
    val r = new scala.util.Random
    (1 to n) map { _ => r.nextInt(max-1) + 1 } toList
  }

  // P25 (*) Generate a random permutation of the elements of a list.
  // Hint: Use the solution of problem P23.
  // Example:
  // scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  // res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  def randomPermute[A](ls: List[A]): List[A] = {
    randomSelect(ls.length, ls)
  }

  // P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  // In how many ways can a committee of 3 be chosen from a group of
  // 12 people? We all know that there are C(12,3) = 220 possibilities
  // (C(N,K) denotes the well-known binomial coefficient). For pure
  // mathematicians, this result may be great. But we want to really
  // generate all the possibilities.
  // Example:
  // scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  // res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  def flatMapSublists[A, B](ls: List[A])(f: List[A] => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map { sl.head :: _ }
    }

  // P27 (**) Group the elements of a set into disjoint subsets.
  // a) In how many ways can a group of 9 people work in 3 disjoint
  // subgroups of 2, 3 and 4 persons? Write a function that generates
  // all the possibilities.
  def group3[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = ls diff a
      b <- combinations(3, noA)
    } yield List(a, b, noA diff b)

  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, ls diff c) map { c :: _ }
    }
  }

  // P28 (**) Sorting a list of lists according to length of sublists.
  //
  // a) We suppose that a list contains elements that are lists
  // themselves. The objective is to sort the elements of the list
  // according to their length. E.g. short lists first, longer lists
  // later, or vice versa.
  // Example:
  // scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g,
  // 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  // res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d,
  // 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i,
  // 'j, 'k, 'l))
  //
  // b) Again, we suppose that a list contains elements that are lists
  // themselves. But this time the objective is to sort the elements
  // according to their length frequency; i.e. in the default, sorting
  // is done ascendingly, lists with rare lengths are placed, others
  // with a more frequent length come later.
  //
  // Note that in the above example, the first two lists in the result
  // have length 4 and 1 and both lengths appear just once. The third
  // and fourth lists have length 3 and there are two list of this
  // length. Finally, the last three lists have length 2. This is the
  // most frequent length.

  def lsort[A](ls: List[List[A]]): List[List[A]] =
    ls sortWith { _.length < _.length }

  def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
    val freqs = Map(encode(ls map { _.length } sortWith { _ < _ }) map { _.swap }:_*)
    ls sortWith { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
  }
}

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isPrime: Boolean = {
      if (start == 2) true
      else if (start % 2 == 0) false
      else (3 to Math.sqrt(start).toInt).forall(start % _ != 0)
    }

    def isPrime_answer: Boolean =
      (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })

    def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1

    def totient: Int = (1 to start) filter { start.isCoprimeTo(_) } length

    def primeFactors: List[Int] = {
      def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] =
        if (n.isPrime) List(n)
        else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
        else primeFactorsR(n, ps.tail)
      primeFactorsR(start, primes)
    }

    def primeFactorMultiplicity: Map[Int, Int] = {
      def inc(m: Map[Int, Int], p: Int): Map[Int, Int] =
        m + (p -> (1 + m.getOrElse(p, 0)))
      def primeFactorsR(n: Int, ps: Stream[Int], m: Map[Int, Int]): Map[Int, Int] =
        if (n.isPrime) inc(m, n)
        else if (n % ps.head == 0) primeFactorsR(n / ps.head, ps, inc(m, ps.head))
        else primeFactorsR(n, ps.tail, m)
      primeFactorsR(start, primes, Map.empty[Int, Int])
    }

    def primeFactorMultiplicity_answer: Map[Int,Int] = {
      def factorCount(n: Int, p: Int): (Int, Int) =
        if (n % p != 0) (0, n)
        else factorCount(n / p, p) match { case (c, d) => (c + 1, d) }
      def factorsR(n: Int, ps: Stream[Int]): Map[Int, Int] =
        if (n == 1) Map()
        else if (n.isPrime) Map(n -> 1)
        else {
          val nps = ps.dropWhile(n % _ != 0)
          val (count, dividend) = factorCount(n, nps.head)
          Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
        }
      factorsR(start, primes)
    }

    def primeFactors2: List[Int] =
      start.primeFactorMultiplicity flatMap { v => List.fill(v._2)(v._1) } toList
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime_answer })

    def gcd(m: Int, n: Int): Int = {
      val h = Math.max(m, n)
      val l = Math.min(m, n)
      val r = h % l

      if (r == 0) l else gcd(l, r)
    }

    def gcd_answer(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
  }
}
