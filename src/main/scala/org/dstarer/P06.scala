package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P06 {
    def isPalindromeBuiltin[A](ls: List[A]): Boolean = {
        ls.equals(ls.reverse)
    }
    
    def isPalindrome[A](ls: List[A]): Boolean = {
        var index = 0
        for (index <- ls.indices) {
            if (ls(index) != ls(ls.length - index - 1)) {
                return false
            }
        }
        true
    }
    
    def isPalindromeRecursive[A](ls: List[A]): Boolean = {
        ls match {
            case Nil => true
            case ls if ls.length == 1 => true
            case head :: tail =>
                if (head.equals(tail.last))
                    return isPalindromeRecursive(tail.init)
                false
            
        }
    }
}
