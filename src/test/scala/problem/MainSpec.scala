import org.scalatest._
import problem.Main._

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
    problem.Main.length(List(1, 1, 2, 3, 5, 8)) should be (6)
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

    problem.Main.drop(3, data) should be (expected)
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
