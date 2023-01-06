@main def solution(params: String*): Unit =
    println(MultiplesOf3Or5.solution(params.head.toInt))


object MultiplesOf3Or5:
    def solution(number: Int): Long =
        val needle = Seq(3, 5)
        val range = 1 to number - 1
        val test: Int => Boolean = (value: Int) => needle.filter(value % _ == 0).length > 0
        range.filter(test).map(_.toLong).sum
