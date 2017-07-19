package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P04 {
    
    def lengthBuiltin(ls: List[Any]): Int = {
        ls.length
    }
    
    def lengthRecursive(ls: List[Any]): Int = {
        ls match {
            case Nil => 0
            case _ :: tail => 1 + lengthRecursive(tail)
        }
    }
    
    def lengthFunction[A](ls: List[A]): Int = {
        ls.foldLeft(0) { (c, _) => c + 1 }
    }
}
