package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P07 {
    def flattenRecursive[A](ls: List[A]): List[A] = {
        ls match {
            case Nil => List()
            case head :: tail => {
                var temp = List[A]()
                if (head.isInstanceOf[List[A]]) {
                    temp = temp ++ flattenRecursive(head.asInstanceOf[List[A]])
                } else {
                    temp = temp :+ head
                }
                val result = temp ::: flattenRecursive(tail)
                result
            }
        }
    }
    
    def flatten(ls: List[Any]): List[Any] = {
        ls flatMap  {
            case ms: List[_] => flatten(ms)
            case e => List(e)
        }
    }
    
}
