package s99.arithmetic

import org.scalatest._
import s99.arithmetic.S99Int
import s99.arithmetic.S99Int._

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

    goldbachList(1 to 2000, 50) should be (Seq(
      (992, (73, 919)), (1382, (61, 1321)), (1856, (67, 1789)), (1928, (61, 1867))
    ))
  }
}
