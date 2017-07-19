package org.dstarer

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
    
    def duplicateElementsNth[A] (ls: List[A], n: Int): List[A] = {
        ls flatMap {
            e => make(n, e)
        }
    }
}
