@main def solution(params: String*): Unit =
    println(moveZeroes(List(1, 0, 1, 2, 0, 1, 3)))
    println(moveZeroes(List(1, 2, 0, 1, 0, 1, 0, 3, 0, 1)))
    println(moveZeroes(List(9, 0, 0, 9, 1, 2, 0, 1, 0, 1, 0, 3, 0, 1, 9, 0, 0, 0, 0, 9)))
    println(moveZeroes(List(0, 0)))
    println(moveZeroes(List()))


def moveZeroes(lst: List[Int]): List[Int] =
    lst.filter(_ > 0) ++ lst.filter(_ == 0)
