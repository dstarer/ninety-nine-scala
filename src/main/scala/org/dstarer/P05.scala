package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P05 {
    def reverseBuiltin[A](ls: List[A]): List[A] = {
        ls.reverse
    }
    
    def reverseRecursive[A](ls: List[A]): List[A] = {
        ls match {
            case Nil => List()
            case head :: tail => reverseRecursive(tail) ::: List(head)
        }
    }
    
    def reverseFunctional[A](ls: List[A]): List[A] = {
        ls.foldRight(List[A]()) {
            (f, z) => {
                z ::: List(f)
            }
        }
    }
    
    def reverseFunctionalLeft[A](ls: List[A]): List[A] = {
        ls.foldLeft(List[A]()) {
            (z, f) => {
                f :: z
            }
        }
    }
}
