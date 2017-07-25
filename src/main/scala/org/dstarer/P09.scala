package org.dstarer

import java.util.NoSuchElementException

/**
  * Created by huberg on 17-7-19.
  */
class P09 {
    def pack[A](ls: List[A]): List[List[A]] = {
        ls.foldRight(List[List[A]]()) {
            (h, r) => {
                if (r.isEmpty || !h.equals(r.head.head)) {
                    List[A](h) :: r
                } else {
                    val c = h :: r.head
                    c :: r.drop(1)
                }
            }
        }
    }

    /**
      *
      * using span filter part the list into two lists.
      * left part is equal to head, ant the other are not
      * span function will stop when encounters a false.
      **/
    def packRecursive[A](ls: List[A]): List[List[A]] = {
        if (ls.isEmpty)
            List(List())
        else {
            val (packed, next) = ls span {
                _ == ls.head
            }
            if (next == Nil)
                List(packed)
            else
                packed :: packRecursive(next)
        }
    }

    def encodeWithRunLength[A](ls: List[A]): List[Any] = {
        if (ls.isEmpty)
            List()
        else {
            val (packed, next) = ls span {
                _ == ls.head
            }
            val left = (packed.length, packed.head)
            if (next == Nil) {
                List(left)
            } else {
                left +: encodeWithRunLength(next)
            }
        }
    }

    def encodeWithSimpleRunLength[A](ls: List[A]): List[Any] = {
        if (ls.isEmpty)
            List()
        else {
            val (packed, next) = ls span {
                _ == ls.head
            }
            val left = {
                if (packed.length > 1)
                    (packed.length, packed.head)
                else
                    packed.head
            }
            if (next == Nil)
                List(left)
            else
                left +: encodeWithSimpleRunLength(next)
        }
    }

    def encodeSimpler[A](ls: List[A]): List[Any] = {
        val packed = packRecursive(ls)
        packed map {
            e => (e.length, e.head)
        }
    }

    def encodefor11[A](ls: List[A]): List[Any] = {
        packRecursive(ls) map {
            e => {
                if (e.length > 1)
                    (e.length, e.head)
                else
                    e.head
            }
        }
    }

    def make[A](count: Int, v: A): List[A] = {
        var ls = List[A]()
        var index: Int = 0
        for (index <- 1 to count) {
            ls = ls :+ v
        }
        ls
    }

    def decode[A](ls: List[(Int, A)]): List[A] = {
        ls flatMap {
            e => make(e._1, e._2)
        }
    }

    def duplicateElements[A](ls: List[A]): List[A] = {
        ls flatMap {
            e => make(2, e)
        }
    }

    def duplicateElementsNth[A](ls: List[A], n: Int): List[A] = {
        ls flatMap {
            e => make(n, e)
        }
    }

    def dropEveryNth[A](n: Int, ls: List[A]): List[A] = {
        val tmpZip1: List[(A, Int)] = ls.zip(ls.indices)
        val tmpZip2: List[(A, Int)] = tmpZip1.filter(a => a._2 % n != n - 1)
        val (res, _) = tmpZip2.unzip
        res
    }

    def dropFunctions[A](n: Int, ls: List[A]): List[A] = {
        ls.zipWithIndex filter {
            v => (v._2 + 1) % n != 0
        } map {
            _._1
        }
    }

    def splitTwoParts[A](n: Int, ls: List[A]): (List[A], List[A]) = {
        val tmp = ls.zipWithIndex partition {
            v => (v._2 < n)
        }
        (tmp._1.map(_._1), tmp._2.map(_._1))
    }

    def splitTwoFunction[A](n: Int, ls: List[A]): (List[A], List[A]) = {
        (ls.take(n), ls.drop(n))
    }

    def extractSlice[A](l: Int, k: Int, ls: List[A]): List[A] = {
        ls match {
            case ls if (ls.length < l) => Nil
            case ls if (ls.length < k) => ls.drop(l)
            case _ => ls.take(k).drop(l)
        }
    }

    def rotate[A](l: Int, ls: List[A]): List[A] = {
        (l, ls) match {
            case (_, Nil) => Nil
            case (l, ls) => {
                if (l == 0) ls
                else if (l < 0) ls.takeRight(-l) ::: ls.dropRight(-l)
                else ls.drop(l) ::: ls.take(l)
            }
        }
    }

    def removeKth[A](l: Int, ls: List[A]): (List[A], A) = {
        if (l < 0)
            throw new NoSuchElementException
        else (l, ls) match {
            case (_, Nil) => throw new NoSuchElementException
            case (0, h :: tail) => (tail, h)
            case (_, h :: tail) => {
                val (t, e) = removeKth(l - 1, tail)
                (h :: t, e)
            }
        }
    }

    def insertKth[A](l: Int, e: A, ls: List[A]): List[A] = {
        if (l > ls.length || l < 0)
            throw new IndexOutOfBoundsException
        (ls.take(l) :+ e) ::: ls.drop(l)
    }

    def range(start: Int, end: Int): List[Int] = {
        if (start > end)
            throw new IllegalArgumentException
        var ls: List[Int] = List[Int]()
        var i = 0
        for (i <- start to (end))
            ls = ls :+ i
        ls
    }

    def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] = {
        f(s) match {
            case None => Nil
            case Some((r, n)) => r :: unfoldRight(n)(f)
        }
    }

    def rangeFunctional(start: Int, end: Int): List[Int] = {
        unfoldRight(start) {
            n =>
                if (n > end) None
                else Some(n, n + 1)
        }
    }

}
