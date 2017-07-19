package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P01 {
    /**
      * find the last element in List
      **/
    def last[A](list: List[A]): A = {
        list.last
    }
    
    def lastRecursive[A](list: List[A]): A = list match {
        case h :: Nil => h
        case _ :: tail => lastRecursive(tail)
        case _ => throw new NoSuchElementException
    }
}
