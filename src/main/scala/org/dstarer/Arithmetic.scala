package org.dstarer

import scala.collection.mutable

class Arithmetic {
    def isPrime(n: Int): Boolean = {
        for (i <- 2 to (Math.sqrt(n).toInt)) {
            if (n % i == 0)
                return false
        }
        true
    }

    def gcd(a: Int, b: Int): Int = {
        (a, b) match {
            case (t, 0) => t
            case (a1, b1) => gcd(b1, a1 % b1)
        }
    }

    def isCoprime(a: Int, b: Int): Boolean = {
        gcd(a, b) == 1
    }

    def totient(a: Int): Int = {
        var count = 0
        for (i <- 1 to (a)) {
            if (gcd(a, i) == 1)
                count += 1
        }
        count
    }

    def totientFunc(a: Int): Int = {
        (1 to a) filter {
            isCoprime(_, a)
        } length
    }

    def primeFactors(a: Int): List[Int] = {
        val factor: List[Int] = (2 to a).toList filter {
            !isCoprime(a, _)
        } filter (isPrime(_))
        var n = a
        factor.foldLeft(List[Int]()) {
            (rs, p) => {
                var result: List[Int] = rs
                while (n % p == 0) {
                    result = result ::: List(p)
                    n = n / p
                }
                result
            }
        }
    }

    def primeFactorMultiplicity(a: Int): List[(Int, Int)] = {
        val factor: List[Int] = (2 to a).toList filter {
            !isCoprime(a, _)
        } filter (isPrime(_))
        var n = a
        factor.foldLeft(List[(Int, Int)]()) {
            (rs, p) => {
                var count = 0
                while (n % p == 0) {
                    count = count + 1
                    n = n / p
                }
                rs :+ (p, count)
            }
        }
    }

}
