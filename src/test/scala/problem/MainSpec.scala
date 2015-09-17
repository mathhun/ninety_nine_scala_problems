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

import problem.arithmetic.S99Int
import problem.arithmetic.S99Int._

class P31Spec extends FlatSpec with Matchers {
  "isPrime" should "determine whether a given integer number is prime" in {
    List(2, 3, 5, 7, 11, 13, 17, 19).foreach(p => {
      p.isPrime should be (true)
      p.isPrime_answer should be (true)
    })

    List(4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20).foreach(n => {
      n.isPrime should be (false)
      n.isPrime_answer should be (false)
    })
  }
}

class P32Spec extends FlatSpec with Matchers {
  "gcd" should "determine the greatest common divisor of two positive integer numbers" in {
    gcd(36, 63) should be (9)
    gcd(63, 36) should be (9)

    gcd_answer(36, 63) should be (9)
    gcd_answer(63, 36) should be (9)
  }
}

class P33Spec extends FlatSpec with Matchers {
  "isCoprimeTo" should "determine whether two positive integer numbers are coprime" in {
    35.isCoprimeTo(64) should be (true)
  }
}

class P34Spec extends FlatSpec with Matchers {
  "totient" should "Calculate Euler's totient function phi(m)." in {
    List((2, 1), (3, 2), (4, 2), (5, 4), (6, 2), (7, 6), (8, 4), (9, 6), (10, 4)) foreach { case (m, n) =>
      m.totient should be (n)
    }
  }
}

class P35Spec extends FlatSpec with Matchers {
  "primeFactors" should "determine the prime factors of a given positive integer" in {
    315.primeFactors should be (List(3, 3, 5, 7))
    315.primeFactors2 should be (List(3, 3, 5, 7)) // using primeFactorMultiplicity
  }
}

class P36Spec extends FlatSpec with Matchers {
  "primeFactorMultiplicity" should "determine the prime factors of a given positive integer (2)" in {
    315.primeFactorMultiplicity should be (Map(3 -> 2, 5 -> 1, 7 -> 1))
    315.primeFactorMultiplicity_answer should be (Map(3 -> 2, 5 -> 1, 7 -> 1))
  }
}

class P37Spec extends FlatSpec with Matchers {
  "totient" should "Calculate Euler's totient function phi(m)." in {
    List((2, 1), (3, 2), (4, 2), (5, 4), (6, 2), (7, 6), (8, 4), (9, 6), (10, 4)) foreach { case (m, n) =>
      m.totient2 should be (n)
    }
  }
}

class P38Spec extends FlatSpec with Matchers {
  //"it" should "Compare the two methods of calculating Euler's totient function." in {
  //  val start1 = System.nanoTime
  //  val res1 = 10090.totient
  //  val end1 = System.nanoTime
  //  val time1 = (end1 - start1).toDouble / 1000.0
  //  println(s"Result: ${res1}  Time:${time1} msec")
  //
  //  val start2 = System.nanoTime
  //  val res2 = 10090.totient2
  //  val end2 = System.nanoTime
  //  val time2 = (end2 - start2).toDouble / 1000.0
  //  println(s"Result: ${res2}  Time:${time2} msec")
  //}

  it should "benchmark totient" in {
    def time[A](label: String)(block: => A): A = {
      val start = System.nanoTime
      val ret = block
      val end = System.nanoTime
      println(label + ": " + ((end - start).toFloat / 1000) + " ms.")
      ret
    }

    def test(n: Int) {
      time("Preload primes") {
        primes takeWhile { _ <= Math.sqrt(n) } force
      }
      time("P34 (" + n + ")") { n.totient }
      time("P37 (" + n + ")") { n.totient2 }
    }

    test(10090)
  }
}

class P39Spec extends FlatSpec with Matchers {
  "listPrimesinRange" should "a list of prime numbers" in {
    listPrimesinRange(7 to 31) should be (List(7, 11, 13, 17, 19, 23, 29, 31))
  }
}

class P40Spec extends FlatSpec with Matchers {
  "goldbach" should "Goldbach's conjecture." in {
    28.goldbach should be (5, 23)
  }
}

class P41Spec extends FlatSpec with Matchers {
  "goldbachList" should "Goldbach's conjecture." in {
    goldbachList(9 to 20) should be (Seq(
      (10, (3, 7)), (12, (5, 7)), (14, (3, 11)), (16, (3, 13)), (18, (5, 13)), (20, (3, 17))
    ))

    //????????????????????
    goldbachList(1 to 2000, 50) should be (Seq(
      (992, (73, 919)), (1382, (61, 1321)), (1856, (67, 1789)), (1928, (61, 1867))
    ))
    //????????????????????
  }
}
