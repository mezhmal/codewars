@main def solution(params: String*): Unit =
    println(Kata.createPhoneNumber(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)))


object Kata:
    def createPhoneNumber(numbers: Seq[Int]): String =
        val n = numbers.mkString
        s"(${n.slice(0, 3)}) ${n.slice(3, 6)}-${n.slice(6, 10)}"
