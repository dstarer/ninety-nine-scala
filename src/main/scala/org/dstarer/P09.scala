package org.dstarer

import java.util.NoSuchElementException
import java.util.Random

import scala.collection.mutable
import scala.reflect.ClassTag

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

    def randomNumbers[A](n: Int, ls: List[A]): List[A] = {
        val seed = new Random()
        n match {
            case 0 => Nil
            case x => {
                val k = seed.nextInt(ls.length)
                val tmp = removeKth(k, ls)
                tmp._2 :: randomNumbers(x - 1, tmp._1)
            }
        }
    }

    def randomSelect[A](n: Int, ls: List[A]): List[A] = {
        def randomSelectR(n: Int, ls: List[A], r: Random): List[A] = {
            if (n <= 0)
                Nil
            else {
                val (rest, e) = removeKth(r.nextInt(ls.length), ls)
                e :: randomSelectR(n - 1, rest, r)
            }
        }

        randomSelectR(n, ls, new Random())
    }

    def lotto(n: Int, bound: Int): List[Int] = {
        randomSelect(n, rangeFunctional(1, bound))
    }

    def randomPermute[A](ls: List[A]): List[A] = {
        randomSelect(ls.length, ls)
    }

    def randomPermuteFisherYates[A: ClassTag](ls: List[A]): List[A] = {
        val rand = new Random()
        val a = ls.toArray
        for (i <- a.length - 1 to 1 by (-1)) {
            val i1 = rand.nextInt(i + 1)
            val t = a(i)
            a.update(i, a(i1))
            a.update(i1, t)
        }
        a.toList
    }

    def combinations[A: ClassTag](K: Int, ls: List[A]): List[List[A]] = {

        def dfs(p: Int, K: Int, ls: List[A], buf: List[A]): List[List[A]] = {
            if (p == ls.length) {
                if (buf.length == K) {
                    return List[List[A]](buf)
                } else {
                    return Nil
                }
            }
            if (p > ls.length)
                return Nil
            if (buf.length == K) {
                return List[List[A]](buf)
            }
            dfs(p + 1, K, ls, (buf :+ ls(p))) ::: dfs(p + 1, K, ls, buf)
        }

        dfs(0, K, ls, List[A]())
    }

    def group3[A: ClassTag](ls: List[A]): List[List[List[A]]] = {
        var res: List[List[List[A]]] = List[List[List[A]]]()
        val a = combinations(2, ls)
        for (tmp <- a) {
            val noA = ls.diff(tmp)
            val b = combinations(3, noA)
            for (t <- b) {
                val c = noA.diff(t)
                res = res :+ List[List[A]](tmp, t, c)
            }
        }
        res
    }

    def groupAccordingList[A: ClassTag](ns: List[Int], ls: List[A]): List[List[List[A]]] = {
        ns match {
            case Nil => List(Nil)
            case n :: tail => combinations(n, ls) flatMap { //generate all possible candidates
                a => // conjunction each candidate
                    groupAccordingList(tail, ls.diff(a)) map {
                        c => a :: c
                    }
            }
        }
    }

    def lsort[A](ls: List[List[A]]): List[List[A]] = {
        ls.sortBy {
            a => a.length
        }
    }

    def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
        val map: mutable.Map[Int, Int] = mutable.Map()
        for (a <- ls) {
            var count = map.getOrElse(a.length, 0)
            count += 1
            map += (a.length -> count)
        }
        ls.sortBy {
            a => map.get(a.length)
        }
    }
}
