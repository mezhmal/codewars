@main def solution(params: String*): Unit =
    println(Dups.duplicateCount("abcdefg"))
    println(Dups.duplicateCount("aabbcdefg"))
    println(Dups.duplicateCount("aBbA"))
    println(Dups.duplicateCount("aA11"))


object Dups:
    def duplicateCount(str: String): Int =
        str.toLowerCase.groupBy(identity).count(_._2.size > 1)
