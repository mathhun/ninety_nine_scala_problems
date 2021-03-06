package s99

import org.scalatest._
import s99.Main._

class P01Spec extends FlatSpec with Matchers {
  "last" should "find the last element of a list" in {
    last(List(1, 1, 2, 3, 5, 8)) should be (8)
  }
}

class P02Spec extends FlatSpec with Matchers {
  "penultimate" should "find the last but one element of a list" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
  }
}

class P03Spec extends FlatSpec with Matchers {
  "nth" should "find the Kth element of a list" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
  }
}

class P04Spec extends FlatSpec with Matchers {
  "length" should "find the number of elements of a list" in {
    s99.Main.length(List(1, 1, 2, 3, 5, 8)) should be (6)
  }
}

class P05Spec extends FlatSpec with Matchers {
  "reverse" should "reverse a list" in {
    reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }
}

class P06Spec extends FlatSpec with Matchers {
  "isPalindrome" should "find out whether a list is a palindrome" in {
    isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
    isPalindrome(List(1, 2, 3, 4, 5)) should be (false)
  }
}

class P07Spec extends FlatSpec with Matchers {
  "flatten" should "flatten a nested list structure" in {
    flatten(List(1,2,3)) should be (List(1,2,3))
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }
}

class P08Spec extends FlatSpec with Matchers {
  "compress" should "eliminate consecutive duplicates of list elements" in {
    val xs = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    compress(xs) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }
}

class P09Spec extends FlatSpec with Matchers {
  "pack" should "pack consecutive duplicates of list elements into sublists" in {
    val xs = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val ys = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    pack(xs) should be (ys)
  }
}

class P10Spec extends FlatSpec with Matchers {
  "encode" should "run-length encoding of a list" in {
    val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    encode(data) should be (expected)
    encode_answer(data) should be (expected)
  }
}

class P11Spec extends FlatSpec with Matchers {
  "encodeModified" should "modified run-length encoding." in {
    val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    encodeModified(data) should be (expected)
  }
}

class P12Spec extends FlatSpec with Matchers {
  "decode" should "decode a run-length encoded list." in {
    val data = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    val expected = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    decode(data) should be (expected)
    decode_answer(data) should be (expected)
  }
}

class P13Spec extends FlatSpec with Matchers {
  "decodeDirect" should "run-length encoding of a list (direct solution)" in {
    val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val expected = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    encodeDirect(data) should be (expected)
  }
}

class P14Spec extends FlatSpec with Matchers {
  "duplicate" should "duplicate the elements of a list" in {
    val expected = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be (expected)
  }
}

class P15Spec extends FlatSpec with Matchers {
  "duplicateN" should "duplicate the elements of a list a given number of times" in {
    val expected = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be (expected)
  }
}

class P16Spec extends FlatSpec with Matchers {
  "drop" should "drop every Nth element from a list" in {
    val data = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val expected = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

    s99.Main.drop(3, data) should be (expected)
    dropRecursive(3, data) should be (expected)
    dropTailRecursive(3, data) should be (expected)
    dropFunctional(3, data) should be (expected)
  }
}

class P17Spec extends FlatSpec with Matchers {
  "split" should "split a list into two parts" in {
    val data = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val expected = (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    split(3, data) should be (expected)
  }
}

class P18Spec extends FlatSpec with Matchers {
  "slice" should "extract a slice from a list" in {
    val data = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val expected = List('d, 'e, 'f, 'g)

    slice(3, 7, data) should be (expected)
    sliceBuiltin(3, 7, data) should be (expected)
  }
}

class P19Spec extends FlatSpec with Matchers {
  "rotate" should "rotate a list N places to the left" in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
}

class P20Spec extends FlatSpec with Matchers {
  "removeAt" should "remove the Kth element from a list" in {
    removeAt(1, List('a, 'b, 'c, 'd)) should be (List('a, 'c, 'd), 'b)
    removeAt(2, List('a, 'b, 'c, 'd)) should be (List('a, 'b, 'd), 'c)

    removeAt_answer(1, List('a, 'b, 'c, 'd)) should be (List('a, 'c, 'd), 'b)
    removeAt_answer(2, List('a, 'b, 'c, 'd)) should be (List('a, 'b, 'd), 'c)
  }
}

class P21Spec extends FlatSpec with Matchers {
  "insertAt" should "insert an element at a given position into a list" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
    insertAt_answer('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
  }
}

class P22Spec extends FlatSpec with Matchers {
  "range" should "create a list containing all integers within a given range" in {
    range(4, 9) should be (List(4, 5, 6, 7, 8, 9))
  }
}

class P23Spec extends FlatSpec with Matchers {
  "randomSelect" should "extract a given number of randomly selected elements from a list" in {
    val data = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val r = randomSelect(3, data)

    r.length should be (3)
    r foreach { e => data.contains(e) should be (true) }
  }
}

class P24Spec extends FlatSpec with Matchers {
  "randomSelect" should "extract a given number of randomly selected elements from a list" in {
    val r = lotto(6, 49)

    r.length should be (6)
    r foreach { e =>
      e should be >= 1
      e should be <= 49
    }
  }
}

class P25Spec extends FlatSpec with Matchers {
  "randomPermute" should "generate a random permutation of the elements of a list" in {
    val data = List('a, 'b, 'c, 'd, 'e, 'f)
    val r = randomPermute(data)

    r.length should be (data.length)
    r should not be (data)
    r.sortBy(_.toString) should be (data.sortBy(_.toString))
  }
}

class P26Spec extends FlatSpec with Matchers {
  "combinations" should "generate the combinations of K distinct objects chosen from the N elements of a list" in {
    val expected = List(List('a, 'b), List('a, 'c), List('b, 'c))
    combinations(2, List('a, 'b, 'c)) should be (expected)
  }
}

class P27Spec extends FlatSpec with Matchers {
  "group" should "" in {
    group(List(1,2), List(1,2,3)) should be (List(
      List(List(1), List(2, 3)),
      List(List(2), List(1, 3)),
      List(List(3), List(1, 2)))
    )

    group(List(2,2), List(1,2,3,4)) should be (List(
      List(List(1, 2), List(3, 4)),
      List(List(1, 3), List(2, 4)),
      List(List(1, 4), List(2, 3)),
      List(List(2, 3), List(1, 4)),
      List(List(2, 4), List(1, 3)),
      List(List(3, 4), List(1, 2)))
    )
  }
}

class P28Spec extends FlatSpec with Matchers {
  val data = List(
    List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)
  )

  "lsort" should "" in {
    lsort(data) should be (List(
      List('o), List('d, 'e), List('d, 'e), List('m, 'n),
      List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    )
  }

  "lsortFreq" should "" in {
    lsortFreq(data) should be (List(
      List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    )
  }
}
