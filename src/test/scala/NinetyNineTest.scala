/**
  * Created by huberg on 17-7-18.
  */

import java.util.NoSuchElementException

import org.dstarer._
import org.scalatest.FunSuite

class NinetyNineTest extends FunSuite {
    test("Test difference") {
        val a = Set("a", "b", "c")
        val b = Set("b", "d")
        assert(a -- b == Set("a", "c"))
    }
    test("P01") {
        val a = new P01()
        val x = List(1, 2, 3, 4)
        assert(a.last(x) == 4)
        assert(a.lastRecursive(x) == 4)
        val c = List(1, "2", "3", "8")
        assert(a.last(c).equals("8"))
        assert(a.lastRecursive(c).equals("8"))
    }
    test("P02") {
        val a = new P02()
        val ls = List(1, 2, 3, 4)
        assert(a.findLastButOne(ls) == 3)
        val t = List(1)
        intercept[NoSuchElementException] {
            a.findLastButOne(t)
        }
    }
    test("p03") {
        val a = new P03()
        val ls = List(1, "2", "3", "5", 4, 7)
        assert(a.findKthElement(ls, 3).equals("3"))
        intercept[NoSuchElementException] {
            a.findKthElement(ls, 7)
        }
        assert(a.kthRecursive(ls, 3).equals("3"))
        intercept[NoSuchElementException] {
            a.kthRecursive(ls, 7)
        }
    }
    test("P04") {
        val a = new P04()
        val ls = List(7, 8, 0, "2", "4", "1")
        assert(a.lengthBuiltin(ls).equals(6))
        assert(a.lengthRecursive(ls).equals(6))
        assert(a.lengthFunction(ls).equals(6))
    }
    test("P05") {
        val a = new P05
        val ls = List(7, 8, 0, "4", "6")
        println(a.reverseRecursive(ls))

        assert(a.reverseRecursive(ls).equals(ls.reverse))

        assert(a.reverseFunctional(ls).equals(ls.reverse))

        assert(a.reverseFunctionalLeft(ls).equals(ls.reverse))
    }

    test("P06") {
        val a = new P06
        val ls1 = List(1, 2, 2, 1)

        assert(a.isPalindromeRecursive(ls1).equals(true))
        val ls2 = List(1, 2, 3, 2, 1)
        assert(a.isPalindromeRecursive(ls2).equals(true))
        var ls3 = List(1, 4, 2, 1)
        assert(a.isPalindromeRecursive(ls3).equals(false))
    }

    test("P07") {
        val a = new P07
        val ls1 = List(List(1, 2), List(3, 4), List(5, List(6, 7)))
        println(a.flattenRecursive(ls1))
        println(ls1.flatten)
        println(a.flatten(ls1))
    }

    test("P08") {
        val a = new P08
        val ls1 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
        println(a compress (ls1))
        println(a compressFunctional ls1)
    }

    test("p09") {
        val a = new P09
        val ls1 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
        println(a pack ls1)
        println(a packRecursive ls1)
    }

    test("P10") {
        val a = new P09
        val ls1 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
        println(a encodeWithRunLength (ls1))
        println(a encodeSimpler (ls1))
    }

    test("p11") {
        val a = new P09
        val ls1 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
        println(a encodeWithSimpleRunLength (ls1))
        println(a encodefor11 (ls1))
    }

    test("p12") {
        val a = new P09
        val ls2 = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
        println(a decode (ls2))
    }

    test("p13") {
        val a = new P09
        val ls1 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
        println(a encodeWithRunLength (ls1))
    }

    test("P14") {
        val a = new P09
        val ls1 = List('a, 'b, 'c, 'c, 'd)
        println(a duplicateElements (ls1))
    }

    test("P15") {
        val a = new P09
        val ls1 = List('a, 'b, 'c, 'c, 'd)
        println(a duplicateElementsNth(ls1, 3))
    }

    test("P16") {
        val a = new P09
        val ls1 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
        println(a dropEveryNth(3, ls1))
    }

    test("P17") {
        val a = new P09
        val lst1 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
        println(a splitTwoParts(3, lst1))
        println(a splitTwoFunction(3, lst1))
    }

    test("P18") {
        val a = new P09
        val lst1 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
        println(a extractSlice(3, 7, lst1))
    }

    test("P19") {
        val a = new P09
        val lst1 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
        println(a rotate(3, lst1))
        println(a rotate(-2, lst1))
    }

    test("P20") {
        val a = new P09
        val lst1 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
        println(a removeKth(2, lst1))
    }

    test("P21") {
        val a = new P09
        val lst1 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
        println(a insertKth(3, 't, lst1))
    }

    test("P22") {
        val a = new P09
        println(a range(4, 9))
        println(a rangeFunctional(4, 9))
    }

    test("P23") {
        val a = new P09
        println(a randomNumbers(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
        println(a randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
    }

    test("P24") {
        val a = new P09
        println(a lotto(6, 49))
    }

    test("p25") {
        val a = new P09
        println(a randomPermute (List('a, 'b, 'c, 'd, 'e, 'f)))
        println(a randomPermuteFisherYates (List('a, 'b, 'c, 'd, 'e, 'f)))
    }

    test("p26") {
        val a = new P09
        println(a combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
    }

    test("P27") {
        val a = new P09
        println(a group3 (List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
        println(a groupAccordingList(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
    }

    test("P28") {
        val a = new P09
        println(a lsort (List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
        println(a lsortFreq (List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
    }

    test("P31") {
        val a = new Arithmetic
        println(a isPrime (7))
    }

    test("P32") {
        val a = new Arithmetic
        println(a gcd(4, 6))
    }
    test("P33") {
        val a = new Arithmetic
        println(a isCoprime(35, 64))
    }
    test("P34") {
        val a = new Arithmetic
        println(a totient (10))
        println(a totientFunc (10))
    }
    test("P35") {
        val a = new Arithmetic
        println(a primeFactors (12))
    }

    test("P36") {
        val a = new Arithmetic
        println(a primeFactorMultiplicity(12))
    }
}

