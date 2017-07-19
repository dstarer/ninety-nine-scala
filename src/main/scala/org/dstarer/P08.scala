package org.dstarer

/**
  * Created by huberg on 17-7-19.
  */
class P08 {
    def compress(ls: List[Any]): List[Any] = {
        var temp = List(ls(0))
        var index = 1
        var pre = ls(0)
        while (index < ls.length - 1) {
            if (!ls(index).equals(pre)) {
                temp = temp :+ ls(index)
                pre = ls(index)
            }
            index += 1
        }
        temp
    }
    
    def compressFunctional[A](ls: List[A]): List[A] = {
        ls.foldRight(List[A]()) {
            (h, r) => {
                if (r.isEmpty || h != r.head)
                    h::r
                else
                    r
            }
        }
    }
}
