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
}

