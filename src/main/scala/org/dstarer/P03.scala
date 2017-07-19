package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P03 {
    def findKthElement[A](ls: List[A], k: Int): A = {
        val length = ls.length
        if (length < k)
            throw new NoSuchElementException
        ls(k - 1)
    }
    
    def kthRecursive[A](ls: List[A], k: Int): A = {
        (ls, k) match {
            case (h :: _, 1) => h
            case (_ :: tail, k) => kthRecursive(tail, k - 1)
            case (Nil, _) => throw new NoSuchElementException
        }
    }
}
