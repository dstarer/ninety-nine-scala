package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P02 {
    /**
      * find the last but one element of a list.
      **/
    def findLastButOne[A](ls: List[A]): A = {
        val c = ls.length
        c match {
            case 0 => throw new NoSuchElementException
            case 1 => throw new NoSuchElementException
            case _ => ls.init.last
        }
    }
}
