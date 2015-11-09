package s99.arithmetic

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

  def totient2: Int =
    start.primeFactorMultiplicity.foldLeft(1)((acc, z) => acc * (z._1 - 1) * Math.pow(z._1, (z._2 - 1)).toInt)

  def totient2_answer: Int =
    start.primeFactorMultiplicity.foldLeft(1) { (r, f) =>
      f match { case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt }
    }

  //P40 (**) Goldbach's conjecture.
  // Goldbach's conjecture says that every positive even number
  // greater than 2 is the sum of two prime numbers. E.g. 28 = 5 +
  // 23. It is one of the most famous facts in number theory that
  // has not been proved to be correct in the general case. It has
  // been numerically confirmed up to very large numbers (much
  // larger than Scala's Int can represent). Write a function to
  // find the two prime numbers that sum up to a given even integer.
  // scala> 28.goldbach
  // res0: (Int, Int) = (5,23)
  def goldbach: (Int, Int) = {
    start.goldbachs.head
  }

  def goldbachs: List[(Int, Int)] = {
    val ps = primes takeWhile { _ < start }
    val pair = ps map { p => (p, start - p) } filter { case (p, n) => ps.contains(n) }
    pair.toList
  }
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

  def listPrimesinRange(range: Range): List[Int] = {
    primes filter { p => range.min <= p && p <= range.max } toList
  }

  // P41 (**) A list of Goldbach compositions.
  def goldbachList(r: Range, lowlimit: Int = 0): Seq[(Int, (Int, Int))] = {
    r filter { n => n > 2 && n % 2 == 0 } map {
      e => (e, e.goldbach)
    } filter { x =>
      x._2._1 > lowlimit
    }
  }
}
